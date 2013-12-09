module Peer.Connection (forkPeer) where

import MetaInfo
import Morphisms
import Peer.Env
import Peer.Get
import Peer.Handshake.Conduit
import Peer.Message.Get
import Peer.Message.Put
import Peer.Request
import Peer.RequestBuffer
import Peer.Send
import Piece
import qualified Torrent as T

import HTorrentPrelude
import Data.Conduit.Network
import Network.Socket

forkPeer :: SockAddr -> ReaderT T.TorrentState IO ()
forkPeer a = ReaderT (void . forkIO . connectPeer a)

connectPeer :: SockAddr -> T.TorrentState -> IO ()
connectPeer a t = do
    s <- socket AF_INET Stream defaultProtocol
    connect s a
    runReaderT (sourceHandshake $$ sinkSocket s) t
    (h, id) <- sourceSocket s $$ sinkHandshake
    when (h == t ^. T.metaInfo . info . hash) (handlePeer s t)

handlePeer :: Socket -> T.TorrentState -> IO ()
handlePeer s t = do
    env <- initPeerEnv
    prs <- newTQueueIO
    pcd <- newTQueueIO
    rs <- newTQueueIO
    prsb <- newEmptyTMVarIO
    let getEnv = GetEnv {   _peerRequests = prs,
                            _peerCancelled = pcd,
                            _peer = env,
                            _torrent = t ^. T.env }
    let sendEnv = SendEnv rs prsb env (t ^. T.env)
    let bufEnv = BufEnv prs pcd prsb
    let requestEnv = RequestEnv rs env (t ^. T.env)
    forkIO (runReaderT requestThread requestEnv)
    forkIO (runReaderT (getThread s) getEnv)
    forkIO (runReaderT (sendThread (t ^. T.numPieces) s) sendEnv)
    runReaderT bufferRequests bufEnv

getThread :: (MonadReader GetEnv m, MonadIO m, MonadThrow m) => Socket -> m ()
getThread s = sourceSocket s $= getMessages $$ handleMessages

sendThread :: (MonadReader SendEnv m, MonadIO m, MonadThrow m) =>
    Int -> Socket -> m ()
sendThread p s = evalStateT (messages $= putMessages p $$ sinkSocket s) st
    where st = SendState { _curInterested = False, _curChoked = True }
