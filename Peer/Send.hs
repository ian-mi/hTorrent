module Peer.Send (SendEnv(SendEnv), SendState(..), messages) where

import Peer.Env
import Peer.Message
import Morphisms
import Torrent.Env

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.Lens
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.STM.Class
import qualified Data.ByteString as BS
import Data.Conduit hiding (Chunk)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

data SendEnv = SendEnv {
    _requests :: TQueue ChunkInd,
    _peerRequests :: TMVar ChunkInd,
    _peer :: PeerEnv,
    _torrent :: TorrentEnv }
$(makeLenses ''SendEnv)

data SendState = SendState {
    _curInterested :: Bool,
    _curChoked :: Bool }
$(makeLenses ''SendState)

data Message = InterestedM | RequestM ChunkInd | PeerRequestM ChunkInd

messages :: (MonadState SendState m, MonadReader SendEnv m, MonadIO m) =>
    Source m PeerMessage
messages = forever (waitMessage >>= sendMessage)

sendMessage :: (MonadState SendState m, MonadReader SendEnv m, MonadIO m) =>
    Message -> Source m PeerMessage
sendMessage InterestedM = do
    i <- use curInterested
    yield (if i then InterestedMessage else UninterestedMessage)
sendMessage (RequestM i) = yield (RequestMessage i)
sendMessage (PeerRequestM c@(ChunkInd p i l)) = do
    d <- IM.lookup p <$> viewTVarIO (torrent . completed)
    maybe (return ()) (yield . PieceMessage . Chunk c . BS.take l . BS.drop i) d
    

waitMessage :: (MonadState SendState m, MonadReader SendEnv m, MonadIO m) =>
    m Message
waitMessage = embedState (embedReader (liftIO . atomically)) $ msum [
    InterestedM <$ waitInterested,
    RequestM <$> waitRequest,
    PeerRequestM <$> waitPeerRequest ]

waitInterested :: (MonadReader SendEnv m, MonadState SendState m, MonadSTM m)
    => m ()
waitInterested = do
    i <- use curInterested
    i' <- viewTVar (peer . peerState . interested)
    liftSTM (guard (not (i == i')))
    curInterested .= i'

waitPeerRequest :: (MonadReader SendEnv m, MonadSTM m) => m ChunkInd
waitPeerRequest = view peerRequests >>= liftSTM . takeTMVar

waitRequest :: (MonadReader SendEnv m, MonadSTM m) => m ChunkInd
waitRequest = view requests >>= liftSTM . readTQueue
