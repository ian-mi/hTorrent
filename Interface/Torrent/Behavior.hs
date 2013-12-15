module Interface.Torrent.Behavior where

import Interface.Completed
import Interface.Peer.Behavior
import Interface.Torrent.Handler
import HTorrentPrelude
import Torrent.Env

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data TorrentBehavior = TorrentBehavior {
    _torrentInfoB :: TorrentInfo,
    _completedBehavior :: Behavior Completed,
    _numCompletedBehavior :: Behavior Int,
    _peerBehavior :: Behavior (HashMap ByteString PeerBehavior)
}
$(makeLenses ''TorrentBehavior)

torrentBehavior :: ReaderT TorrentEnv IO (TorrentBehavior, TorrentHandlerEnv)
torrentBehavior = do
    chan <- view torrentEvents >>= liftIO . atomically . dupTChan
    env <- ask
    (completedB, completedH) <- startCompleted
    (numCompletedB, numCompletedH) <- startCompletedNum
    startPeers <- viewTVarIO peers
    (peersInit, peersH) <- liftIO (mkPeersBehavior startPeers)
    (peerConnectE, peerConnectH) <- liftIO newEvent
    peersB <- accumB peersInit (uncurry insertMap <$> peerConnectE)
    let torrentBehavior = TorrentBehavior {
        _torrentInfoB = env ^. torrentInfo,
        _completedBehavior = completedB,
        _numCompletedBehavior = numCompletedB,
        _peerBehavior = peersB
    }
    let torrentH = TorrentHandler {
        _completedHandler = completedH,
        _numCompletedHandler = numCompletedH,
        _peerConnectHandler = peerConnectH
    }
    let torrentHandlerEnv = TorrentHandlerEnv {
        _torrentHandler = torrentH,
        _peerHandlersInit = peersH,
        _torrentEnv = env,
        _torrentChan = chan
    }
    return (torrentBehavior, torrentHandlerEnv)
