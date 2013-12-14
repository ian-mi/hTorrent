module Peer.Connection (peerThread) where

import HTorrentPrelude
import MetaInfo
import Morphisms
import Peer.Env
import Peer.Get
import Peer.Handshake
import Peer.Message.Put
import Peer.Request
import Peer.RequestBuffer
import Peer.Send
import Piece
import qualified Torrent.State as T
import Torrent.Info

import Data.Conduit.Network
import Network.Socket

data PeerConnectionException =
    FailedHandshake HandshakeException
    deriving Show

type ConnectionIO a = ExceptionalT PeerConnectionException IO a

peerThread :: T.TorrentState -> SockAddr -> IO ()
peerThread t a = do
    s <- connectPeer a
    r <- tryT (handlePeer t s)
    case r of
        Success () -> return ()
        Exception e -> do
            putStrLn "Peer connection exception"
    close s

connectPeer :: SockAddr -> IO Socket
connectPeer a = do
    s <- socket AF_INET Stream defaultProtocol
    connect s a
    return s

handlePeer :: T.TorrentState -> Socket -> ConnectionIO ()
handlePeer t s = do
    env <- mapExceptionT FailedHandshake (handshake t s)
    lift $ do
        prs <- newTQueueIO
        pcd <- newTQueueIO
        rs <- newTQueueIO
        prsb <- newEmptyTMVarIO
        let getEnv = GetEnv prs pcd env (t ^. T.env)
        let sendEnv = SendEnv rs prsb env (t ^. T.env)
        let bufEnv = BufEnv prs pcd prsb
        let requestEnv = RequestEnv rs env (t ^. T.env)
        forkIO (runReaderT requestThread requestEnv)
        forkIO (runReaderT (getThread s) getEnv)
        forkIO (runReaderT (sendThread (t ^. T.torrentInfo . numPieces) s) sendEnv)
        runReaderT bufferRequests bufEnv

sendThread :: (MonadReader SendEnv m, MonadIO m, MonadThrow m) =>
    Int -> Socket -> m ()
sendThread p s = evalStateT (messages $= putMessages p $$ sinkSocket s) st
    where st = SendState { _curInterested = False, _curChoked = True }
