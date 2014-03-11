module Peer.Connection (peerThread) where

import HTorrentPrelude
import Morphisms
import Peer.Env
import Peer.Get
import Peer.Handshake
import Peer.Request
import Peer.RequestBuffer
import Peer.Send
import Peer.State
import Peer.Message
import Torrent.Env

import Control.Concurrent.Async
import Network.Socket

data PeerConnectionExcept =
    FailedHandshake HandshakeExcept
    deriving Show

peerThread :: TorrentEnv -> SockAddr -> IO ()
peerThread t a = do
    st <- initPeerState a
    r <- try (hoist (flip runReaderT (PeerEnv t st)) runPeer)
    return ()

runPeer :: ExceptT PeerConnectionExcept (ReaderT PeerEnv IO) ()
runPeer = bracket (lift connectPeer) (lift . lift . close) handlePeer

connectPeer :: ReaderT PeerEnv IO Socket
connectPeer = do
    s <- liftIO (socket AF_INET Stream defaultProtocol)
    view (peerState . address) >>= liftIO . connect s
    return s

handlePeer :: Socket -> ExceptT PeerConnectionExcept (ReaderT PeerEnv IO) ()
handlePeer s = do
    mapExceptT FailedHandshake (handshake s)
    env <- ask
    lift $ lift $ do
        prs <- newTQueueIO
        pcd <- newTQueueIO
        rs <- newTQueueIO
        prsb <- newEmptyTMVarIO
        let getEnv = GetEnv prs pcd env
        let sendEnv = SendEnv rs prsb env
        let bufEnv = BufEnv prs pcd prsb
        threads <- mapM async [
            runReaderT (requestThread rs) env,
            getThread getEnv s,
            runReaderT (sendThread  s) sendEnv,
            runReaderT bufferRequests bufEnv ]
        waitAnyCancel threads
    return ()
