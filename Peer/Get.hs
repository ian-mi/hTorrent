module Peer.Get (getThread, GetEnv(..)) where

import Peer.Env
import Peer.Event
import Peer.Message
import Peer.Message.Get
import Piece
import Morphisms
import Torrent.Env
import Torrent.Event

import HTorrentPrelude
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Network.Socket

data GetEnv = GetEnv {
    _peerRequests :: TQueue ChunkInd,
    _peerCancelled :: TQueue ChunkInd,
    _peer :: PeerEnv,
    _torrent :: TorrentEnv }
$(makeLenses ''GetEnv)

getThread :: Socket -> ReaderT GetEnv IO ()
getThread s = sourceSocket s $= getMessages $$ handleMessages

handleMessages :: Consumer PeerMessage (ReaderT GetEnv IO) ()
handleMessages = CL.mapM_ handlePeerMessage

handlePeerMessage :: PeerMessage -> ReaderT GetEnv IO ()
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
handlePeerMessage (HaveMessage i) = peer . pieces !%= insertSet i
handlePeerMessage (BitfieldMessage b) = peer . pieces !.= b
handlePeerMessage (RequestMessage r) = 
    view peerRequests >>= liftIO . atomically . flip writeTQueue r
handlePeerMessage (PieceMessage (Chunk i d)) = do
    peer . pendingRequests !%= deleteSet i
    hoist atomically (magnify torrent (addPiece i d))
handlePeerMessage (CancelMessage c) = 
    view peerCancelled >>= liftIO . atomically . flip writeTQueue c

addPiece :: ChunkInd -> ByteString -> ReaderT TorrentEnv STM ()
addPiece ind@(ChunkInd p i l) d = void $ runMaybeT $ do
    buf <- MaybeT (liftM (lookup p) (viewTVar downloading))
    lift (addChunk buf (i, d))
    bs <- MaybeT (liftSTM (complete <$> readTVar buf))
    lift (completePiece p bs)

completePiece :: Int -> ByteString -> ReaderT TorrentEnv STM ()
completePiece pieceNumber pieceData = do
    downloading &%= deleteMap pieceNumber
    completed &%= insertMap pieceNumber pieceData
    numCompleted &%= succ
    torrentEvents &-< PieceCompleted pieceNumber

addChunk :: MonadSTM m => TVar PieceBuffer -> (Int, ByteString) -> m ()
addChunk buf =  liftSTM . modifyTVar buf . execState . addData
