module Peer.Connection (forkPeer) where

import HTorrentPrelude
import MetaInfo
import Morphisms
import Peer.Env
import Peer.Get
import Peer.Handshake
import Peer.Message.Get
import Peer.Message.Put
import Peer.Request
import Peer.RequestBuffer
import Peer.Send
import Piece
import qualified Torrent.State as T

import Data.Conduit.Network
import Network.Socket

data PeerConnectionException =
    FailedHandshake HandshakeException
    deriving Show

type ConnectionIO a = ExceptionalT PeerConnectionException IO a

forkPeer :: T.TorrentState -> SockAddr -> IO PeerEnv
forkPeer t a = do
    env <- initPeerEnv
    forkIO (peerThread a t env)
    return env

peerThread :: SockAddr -> T.TorrentState -> PeerEnv -> IO ()
peerThread a t env = do
    s <- connectPeer a
    r <- tryT (handlePeer t env s)
    case r of
        Success () -> return ()
        Exception e -> do
            putStrLn "Peer connection exception: "
            print e
    close s

connectPeer :: SockAddr -> IO Socket
connectPeer a = do
    s <- socket AF_INET Stream defaultProtocol
    connect s a
    return s

handlePeer :: T.TorrentState -> PeerEnv -> Socket -> ConnectionIO ()
handlePeer t env s = do
    id <- mapExceptionT FailedHandshake (handshake t s)
    lift $ do
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
