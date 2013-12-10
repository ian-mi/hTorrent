module Peer.Get (GetEnv(..), handleMessages) where

import Peer.Env
import Peer.Event
import Peer.Message
import Piece
import Morphisms
import Torrent.Env
import Torrent.Event

import HTorrentPrelude
import Control.Monad.STM.Class
import qualified Data.Conduit.List as CL
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S

data GetEnv = GetEnv {
    _peerRequests :: TQueue ChunkInd,
    _peerCancelled :: TQueue ChunkInd,
    _peer :: PeerEnv,
    _torrent :: TorrentEnv }
$(makeLenses ''GetEnv)

handleMessages :: (MonadReader GetEnv m, MonadIO m) => Consumer PeerMessage m ()
handleMessages = CL.mapM_ handlePeerMessage

handlePeerMessage :: (MonadReader GetEnv m, MonadIO m) => PeerMessage -> m ()
handlePeerMessage ChokeMessage = do
    peer . remoteState . choked !.= True
    events <- view (peer . peerEvents)
    liftIO (atomically (writeTChan events (ChokedRemote True)))
handlePeerMessage UnchokeMessage = do
    peer . remoteState . choked !.= False
    events <- view (peer . peerEvents)
    liftIO (atomically (writeTChan events (ChokedRemote False)))
handlePeerMessage InterestedMessage = do
    peer . remoteState . interested !.= True
    events <- view (peer . peerEvents)
    liftIO (atomically (writeTChan events (InterestedRemote True)))
handlePeerMessage UninterestedMessage = do
    peer . remoteState . interested !.= False
    events <- view (peer . peerEvents)
    liftIO (atomically (writeTChan events (InterestedRemote False)))
handlePeerMessage (HaveMessage i) = peer . pieces !%= IS.insert i
handlePeerMessage (BitfieldMessage b) = peer . pieces !.= b
handlePeerMessage (RequestMessage r) = 
    view peerRequests >>= liftIO . atomically . flip writeTQueue r
handlePeerMessage (PieceMessage (Chunk ind@(ChunkInd p i l) d)) =
    embedReader (liftIO . atomically) $ void $ runMaybeT $ do
        peer . pendingRequests &%= S.delete ind
        buf <- MaybeT (liftM (IM.lookup p) (viewTVar (torrent . downloading)))
        lift (addChunk buf (i, d))
        bs <- MaybeT (liftSTM (complete <$> readTVar buf))
        lift (completePiece p bs)
handlePeerMessage (CancelMessage c) = 
    view peerCancelled >>= liftIO . atomically . flip writeTQueue c

completePiece :: Int -> ByteString -> ReaderT GetEnv STM ()
completePiece pieceNumber pieceData = do
    torrent . downloading &%= IM.delete pieceNumber
    torrent.completed &%= IM.insert pieceNumber pieceData
    events <- view (torrent . torrentEvents)
    liftSTM (writeTChan events (PieceCompleted pieceNumber))

addChunk :: MonadSTM m => TVar PieceBuffer -> (Int, ByteString) -> m ()
addChunk buf =  liftSTM . modifyTVar buf . execState . addData
