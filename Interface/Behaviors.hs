module Interface.Behaviors (
    mkBehaviors,
    Behaviors(..),
    completedB,
    peerB ) where

import Interface.Completed
import Interface.Peer
import HTorrentPrelude
import Torrent.Env
import Torrent.Event
import Torrent.State

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data Behaviors = Behaviors {
    _completedB :: Behavior Completed,
    _peerB :: Behavior Peers
}
$(makeLenses ''Behaviors)

data Handlers = Handlers {
    _completedH :: Handler Completed,
    _peersH :: STM (IO ())
}
$(makeLenses ''Handlers)

mkBehaviors :: TorrentState -> IO (Behaviors, IO ())
mkBehaviors ts = do
    chan <- atomically (dupTChan (ts ^. env . torrentEvents))
    (completedBehavior, completedHandler) <- startCompleted ts
    startPeers <- readTVarIO (ts ^. peers)
    (peersBehavior, peersHandler) <- mkPeersBehavior startPeers
    let behaviors = Behaviors {
            _completedB = completedBehavior,
            _peerB = pure (peersBehavior)
        }
    let handlers = Handlers {
        _completedH = completedHandler,
        _peersH = peersHandler
    }
    return (behaviors, runHandlers ts handlers chan)

runTorrentEvents :: TorrentState -> Handlers -> TChan TorrentEvent -> STM (IO ())
runTorrentEvents ts hs chan = do
    event <- readTChan chan
    case event of
        PieceCompleted pieceNum ->
            hs ^. completedH <$> readTVar (ts ^. env . completed)

runHandlers :: TorrentState -> Handlers -> TChan TorrentEvent -> IO ()
runHandlers ts hs chan = forever (join (atomically stm))
    where stm = msum [runTorrentEvents ts hs chan, hs ^. peersH]
