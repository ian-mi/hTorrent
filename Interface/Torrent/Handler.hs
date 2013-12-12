module Interface.Torrent.Handler where

import Interface.Completed
import Interface.Peer
import Interface.Peer.Behavior
import Interface.Peer.Handler
import HTorrentPrelude
import Torrent.Env
import Torrent.Event

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data TorrentHandler = TorrentHandler {
    _completedHandler :: Handler Completed,
    _peerConnectHandler :: Handler (ByteString, PeerBehavior)
}
$(makeLenses ''TorrentHandler)

data TorrentHandlerEnv = TorrentHandlerEnv {
    _torrentHandler :: TorrentHandler,
    _peerHandlersInit :: HashMap ByteString PeerHandlerEnv,
    _torrentEnv :: TorrentEnv,
    _torrentChan :: TChan TorrentEvent
}
$(makeLenses ''TorrentHandlerEnv)

newtype TorrentHandlerState = TorrentHandlerState {
    _peerThreads :: HashMap ByteString ThreadId
}
$(makeLenses ''TorrentHandlerState)

runTorrentHandlerInit :: ReaderT TorrentHandlerEnv IO ()
runTorrentHandlerInit = do
    ps <- view peerHandlersInit
    initPeerThreads <- mapMOf traversed (liftIO . forkIO . handlePeer) ps
    let torrentHandlerState = TorrentHandlerState initPeerThreads
    evalStateT runTorrentHandler torrentHandlerState

runTorrentHandler :: StateT TorrentHandlerState (ReaderT TorrentHandlerEnv IO) ()
runTorrentHandler = forever $ do
    event <- viewTChanIO torrentChan
    case event of
        PieceCompleted pieceNum -> do
            h <- view (torrentHandler . completedHandler)
            c <- viewTVarIO (torrentEnv . completed)
            liftIO (h c)
        PeerConnected id env -> do
            (behavior, handler) <- liftIO (mkPeerBehavior env)
            threadId <- liftIO (forkIO (handlePeer handler))
            peerThreads %= insertMap id threadId
            h <- view (torrentHandler . peerConnectHandler)
            liftIO (h (id, behavior))
