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
import Torrent.Event

import Control.Concurrent.Async
import Network.Socket

data PeerConnectionExcept =
    FailedHandshake HandshakeExcept
    deriving Show

peerThread :: TorrentEnv -> SockAddr -> IO ()
peerThread env a = do
    s <- connectPeer a
    runPeer env a s
    close s

runPeer :: TorrentEnv -> SockAddr -> Socket -> IO ()
runPeer tEnv a s = void $ try $ do
        id <- mapExceptT FailedHandshake (handshake s (tEnv ^. torrentInfo))
        st <- lift (initPeerState a id)
        let env = PeerEnv tEnv st
        mapExceptT FailedHandshake (runReaderT (addPeer id st) tEnv)
        lift $ do
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

connectPeer :: SockAddr -> IO Socket
connectPeer a = do
    s <- socket AF_INET Stream defaultProtocol
    s <$ connect s a

addPeer :: ByteString -> PeerState -> ReaderT TorrentEnv (ExceptT HandshakeExcept IO) ()
addPeer id st = hoist (hoist atomically) $ do
    ps <- viewTVar peers
    assert (InvalidId id) (not (member id ps))
    peers &.= insertMap id st ps
    torrentEvents &-< PeerConnected id st
