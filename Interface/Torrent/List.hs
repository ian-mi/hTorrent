module Interface.Torrent.List (torrentList) where

import HTorrentPrelude
import Interface.Torrent.Behavior
import Interface.Torrent.Entry

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

torrentList :: [TorrentBehavior] -> UI (Element, Behavior (Maybe TorrentBehavior))
torrentList torrents = do
    (elements, events) <- (map fst &&& map snd) <$> mapM mkEvent torrents
    let clicked = foldr (UI.unionWith const) never events
    tableElement <- UI.table #+ (torrentListHeader : (UI.element <$> elements))
    focusB <- stepper Nothing (Just <$> clicked)
    return (tableElement, focusB)
    where   mkEvent b = do
                e <- torrentEntry b
                return (e, b <$ UI.click e)

torrentListHeader :: UI Element
torrentListHeader = UI.tr #+ [nameH, progressH]
    where   nameH = UI.set text "Torrent Name" UI.th
            progressH = UI.set text "Progress" UI.th
