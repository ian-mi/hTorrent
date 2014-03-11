module Peer.Send (SendEnv(SendEnv), sendThread) where

import Data.Chunk
import Peer.Env
import Peer.Message
import Peer.Message.Put
import Peer.State
import Morphisms
import Torrent.Env

import HTorrentPrelude
import Control.Monad.STM.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Network
import Data.Interval
import qualified Data.IntMap as IM
import Network.Socket

data SendEnv = SendEnv {
    _requests :: TQueue Chunk,
    _peerRequests :: TMVar Chunk,
    _peer :: PeerEnv }
$(makeLenses ''SendEnv)

data SendState = SendState {
    _curInterested :: Bool,
    _curChoked :: Bool }
$(makeLenses ''SendState)

data Message = InterestedM | RequestM Chunk | PeerRequestM Chunk

sendThread :: (MonadReader SendEnv m, MonadIO m, MonadThrow m) => Socket -> m ()
sendThread s = do
    p <- view (peer . torrentEnv . torrentInfo . numPieces)
    evalStateT (messages $= putMessages p $$ sinkSocket s) st
    where st = SendState { _curInterested = False, _curChoked = True }

messages :: (MonadState SendState m, MonadReader SendEnv m, MonadIO m) =>
    Source m PeerMessage
messages = forever (waitMessage >>= sendMessage)

sendMessage :: (MonadState SendState m, MonadReader SendEnv m, MonadIO m) =>
    Message -> Source m PeerMessage
sendMessage InterestedM = do
    i <- use curInterested
    yield (if i then InterestedMessage else UninterestedMessage)
sendMessage (RequestM c) = yield (RequestMessage c)
sendMessage (PeerRequestM c@(Chunk p i)) = do
    r <- IM.lookup p <$> viewTVarIO (peer . torrentEnv . completed)
    case r of
        Nothing -> return ()
        Just (takeInterval i -> d) -> yield (PieceMessage c d)

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
    i' <- viewTVar (peer . peerState . remoteState . interested)
    liftSTM (guard (not (i == i')))
    curInterested .= i'

waitPeerRequest :: (MonadReader SendEnv m, MonadSTM m) => m Chunk
waitPeerRequest = view peerRequests >>= liftSTM . takeTMVar

waitRequest :: (MonadReader SendEnv m, MonadSTM m) => m Chunk
waitRequest = view requests >>= liftSTM . readTQueue

takeInterval :: Interval -> ByteString -> ByteString
takeInterval (Interval a b) = take (b - a + 1) . drop a
