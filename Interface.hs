module Interface (startInterface) where

import Interface.Torrent.Behavior
import Interface.Torrent.Handler
import Interface.Completed
import Interface.Peer
import HTorrentPrelude
import MetaInfo
import Torrent.Env
import Torrent.State

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

startInterface :: TorrentState -> IO ()
startInterface ts = do
    (torrentBehavior, torrentHandlerEnv) <- runReaderT torrentBehavior (ts ^. env)
    forkIO (runReaderT runTorrentHandlerInit torrentHandlerEnv)
    startGUI config (interface torrentBehavior)
    where config = defaultConfig { tpPort = 10000 }

interface :: TorrentBehavior -> Window -> UI ()
interface b w = do
    UI.set UI.title "hTorrent" (return w)
    void (getBody w #+ [
        completedUI (b ^. completedBehavior),
        peerUI (b ^. peerBehavior) ])

torrentInfo :: MonadReader TorrentState m => m (UI Element)
torrentInfo = do
    t <- liftM string (view (metaInfo . info . name))
    i <- sequence [
        displayPieces,
        displayPieceLength ]
    return (t #+ [UI.ul #+ map (UI.li #+) i])

displayPieces :: MonadReader TorrentState m => m [UI Element]
displayPieces = do
    p <- view numPieces
    return [string "Pieces:", string (show p)]

displayPieceLength :: MonadReader TorrentState m => m [UI Element]
displayPieceLength = do
    l <- view (metaInfo . info . piece_length)
    return [string "Piece Length:", string (show l) ]
