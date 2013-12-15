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
import Piece
import Torrent.Env

import Network.Socket

data PeerConnectionExcept =
    FailedHandshake HandshakeExcept
    deriving Show

peerThread :: TorrentEnv -> SockAddr -> IO ()
peerThread t a = do
    st <- initPeerState a
    r <- try (hoist (flip runReaderT (PeerEnv t st)) runPeer)
    case r of
        Success () -> return ()
        Exception e -> do
            putStrLn "Peer connection exception"

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
        let requestEnv = RequestEnv rs env
        forkIO (runReaderT requestThread requestEnv)
        forkIO (runReaderT (getThread s) getEnv)
        forkIO (runReaderT (sendThread  s) sendEnv)
        runReaderT bufferRequests bufEnv
