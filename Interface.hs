module Interface(startInterface) where

import Common
import Interface.Expand
import MetaInfo
import Torrent
import Torrent.Env

import qualified Data.IntMap as IM
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data Behaviors = Behaviors { _completedB :: Behavior (IntMap ByteString) }
$(makeLenses ''Behaviors)

startInterface :: TorrentState -> IO ()
startInterface ts = do
    (completedEvent, completedHandler) <- newEvent
    completedStart <- readTVarIO (ts ^. env . completed)
    completedBehavior <- stepper completedStart completedEvent
    let b = Behaviors { _completedB = completedBehavior }
    forkIO $ forever $ do
        atomically (takeTMVar (ts ^. env . completedSig))
        readTVarIO (ts ^. env . completed) >>= completedHandler
    startGUI config (interface b)
    where config = defaultConfig { tpPort = 10000 }

interface :: Behaviors -> Window -> UI ()
interface b w = do
    UI.set UI.title "hTorrent" (return w)
    void (getBody w #+ [completedUI b])

completedUI :: Behaviors -> UI Element
completedUI b = do
    name <- string "Completed"
    completedTable <- sink completedAttr (b ^. completedB) UI.table
    expand name [completedTable]
    where completedAttr = mkWriteAttr updateCompleted

updateCompleted :: IntMap ByteString -> Element -> UI ()
updateCompleted c t = do
    cells <- mapM f (IM.keys c)
    void (UI.set UI.children cells (UI.element t))
    where f i = UI.set text (show i) UI.td

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
