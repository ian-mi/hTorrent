module Interface.Torrent.Details (details)where

import Interface.Torrent.Behavior
import HTorrentPrelude
import Torrent.Env

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Network.URI
import Reactive.Threepenny

details :: TorrentBehavior -> UI Element
details b = table #. "details"
    where
        table =  UI.table #+ [
                nameRow (b ^. torrentInfoB . torrentName),
                trackerRow (b ^. torrentInfoB . tracker),
                pieceRow (b ^. torrentInfoB  . numPieces),
                pieceSizeRow (b ^. torrentInfoB . pieceLength),
                completedRow (b ^. numCompletedBehavior)
            ]

nameRow :: String -> UI Element
nameRow name = UI.tr #+ [c1, c2]
    where
        c1 = UI.set text "Torrent Name" UI.td
        c2 = UI.set text name UI.td

trackerRow :: URI -> UI Element
trackerRow u = UI.tr #+ [c1, c2]
    where
        c1 = UI.set text "Tracker URI" UI.td
        c2 = UI.set text (show u) UI.td

pieceRow :: Int -> UI Element
pieceRow i = UI.tr #+ [c1, c2]
    where
        c1 = UI.set text "Pieces" UI.td
        c2 = UI.set text (show i) UI.td

pieceSizeRow :: Int -> UI Element
pieceSizeRow i = UI.tr #+ [c1, c2]
    where
        c1 = UI.set text "Piece Size" UI.td
        c2 = UI.set text (show i) UI.td

completedRow :: Behavior Int -> UI Element
completedRow i = UI.tr #+ [c1, c2]
    where
        c1 = UI.set text "Completed Pieces" UI.td
        c2 = UI.sink text (show <$> i) UI.td
