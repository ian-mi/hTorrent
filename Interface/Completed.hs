module Interface.Completed (startCompleted, completedUI, Completed) where

import Interface.Expand
import HTorrentPrelude
import Torrent.Env

import qualified Data.IntMap as IM
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

type Completed = IntMap ByteString

startCompleted :: ReaderT TorrentEnv IO (Behavior Completed, Handler Completed)
startCompleted = do
    (completedEvent, completedHandler) <- liftIO (newEvent)
    completedStart <- viewTVarIO completed
    completedBehavior <- liftIO (stepper completedStart completedEvent)
    return (completedBehavior, completedHandler)

completedUI :: Behavior Completed -> UI Element
completedUI b = do
    name <- string "Completed"
    completedTable <- sink completedAttr b UI.table
    expand name [completedTable]
    where completedAttr = mkWriteAttr updateCompleted

updateCompleted :: Completed -> Element -> UI ()
updateCompleted c t = do
    cells <- mapM f (IM.keys c)
    void (UI.set UI.children cells (UI.element t))
    where f i = UI.set text (show i) UI.td
