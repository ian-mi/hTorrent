module Interface.Torrent.Info where

import Interface.Completed
import Interface.Peer
import Interface.Torrent.Behavior
import Interface.Torrent.Details
import HTorrentPrelude

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data InfoSelect = Details | Peers deriving Eq

selectTorrentInfo :: Maybe TorrentBehavior -> InfoSelect -> Element -> UI ()
selectTorrentInfo Nothing _ e = void (UI.set UI.children [] (UI.element e))
selectTorrentInfo (Just b) Details e = do
    d <- details b
    void (UI.set UI.children [d] (UI.element e))
selectTorrentInfo (Just b) Peers e = do
    p <- b ^! peerBehavior . act peerUI
    void (UI.set UI.children [p] (UI.element e))

torrentInfoFocus :: Behavior (Maybe TorrentBehavior) -> UI Element
torrentInfoFocus b = do
    (menu, select) <- torrentInfoMenu
    body <- sink attr ((,) <$> b <*> select) UI.div
    UI.div #+ [UI.element menu, UI.element body]
    where attr = mkWriteAttr (uncurry selectTorrentInfo)

torrentInfoMenu :: UI (Element, Behavior InfoSelect)
torrentInfoMenu = do
    details <- UI.set text "Details" UI.td
    peers <- UI.set text "Peers" UI.td
    let detailsE = Details <$ UI.click details
    let peersE = Peers <$ UI.click peers
    b <- stepper Details (unionF [detailsE, peersE])
    let cells = [   sink UI.class_ (f Details <$> b) (UI.element details),
                    sink UI.class_ (f Peers <$> b) (UI.element peers) ]
    menu <- UI.div #+ [UI.table #+ cells] #.  "torrentInfoMenu"
    return (menu , b)
    where   f :: InfoSelect -> InfoSelect -> String
            f x y = if x == y then "selected" else ""


unionF :: [Event a] -> Event a
unionF = foldr (UI.unionWith const) never
