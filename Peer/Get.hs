module Peer.Get (GetEnv(..), handleMessages) where

import Common
import Peer.Env
import Peer.Message
import Piece
import Morphisms
import Torrent.Env

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
handlePeerMessage ChokeMessage = peer . peerState . choked !.= True
handlePeerMessage UnchokeMessage = peer . peerState . choked !.= False
handlePeerMessage InterestedMessage = peer . peerState . interested !.= True
handlePeerMessage UninterestedMessage = peer . peerState . interested !.= False
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
        torrent . downloading &%= (IM.delete p)
        torrent . completed &%= IM.insert p bs
        view (torrent . completedSig) >>= liftSTM . void . flip tryPutTMVar ()

handlePeerMessage (CancelMessage c) = 
    view peerCancelled >>= liftIO . atomically . flip writeTQueue c

addChunk :: MonadSTM m => TVar PieceBuffer -> (Int, ByteString) -> m ()
addChunk buf =  liftSTM . modifyTVar buf . execState . addData
