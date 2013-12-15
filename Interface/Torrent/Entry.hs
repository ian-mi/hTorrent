module Interface.Torrent.Entry (torrentEntry) where

import HTorrentPrelude
import Interface.Torrent.Behavior
import Torrent.Env

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

torrentEntry :: TorrentBehavior -> UI Element
torrentEntry b = do
    name <- UI.set text (b ^. torrentInfoB . torrentName) UI.td
    progress <- mkElement "progress"
    UI.set (attr "max") (show (b ^. torrentInfoB . numPieces)) (UI.element progress)
    sink value (show <$> b ^. numCompletedBehavior) (UI.element progress)
    UI.tr #+ [UI.element name, UI.td #+ [UI.element progress]]
