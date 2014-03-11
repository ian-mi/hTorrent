module Interface (startInterface) where

import Interface.Torrent.Info
import Interface.Torrent.List
import Interface.Torrent.Behavior
import Interface.Torrent.Handler
import Interface.Completed
import Interface.Peer
import HTorrentPrelude
import MetaInfo
import Torrent.Env

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

startInterface :: TorrentEnv -> IO ()
startInterface env = do
    let i = env
    (torrentBehavior, torrentHandlerEnv) <- runReaderT torrentBehavior env
    forkIO (runReaderT runTorrentHandlerInit torrentHandlerEnv)
    startGUI config (interface torrentBehavior)
    where config = defaultConfig {tpPort = 10000, tpStatic = Just "static"}

interface :: TorrentBehavior -> Window -> UI ()
interface b w = do
    UI.addStyleSheet w "hTorrent.css"
    UI.set UI.title "hTorrent" (return w)
    (torrentTable, torrentFocusB) <- torrentList [b]
    torrentFocus <- torrentInfoFocus torrentFocusB
    let body = UI.set style bodyStyle (getBody w)
    void (body #+ layout torrentTable torrentFocus)
    where bodyStyle = [ ("padding", "0"),
                        ("margin", "0"),
                        ("height", "100%"),
                        ("min-height", "100%") ]

layout :: Element -> Element -> [UI Element]
layout torrentTable torrentInfo = [body, footer]
    where   bodyStyle = []
            bodyDiv = UI.set style bodyStyle UI.div
            body = bodyDiv #+ [UI.element torrentTable]
            footerDiv = UI.set style footerStyle UI.div
            footer = footerDiv #+ [UI.element torrentInfo]

footerStyle :: [(String, String)]
footerStyle = [
    ("position", "fixed"),
    ("left", "0"),
    ("bottom", "0"),
    ("height", "40%"),
    ("width", "100%") ]
