module Interface.Peer (peerUI, PeersB) where

import Interface.Expand
import Interface.Peer.Behavior
import Interface.Peer.Handler
import HTorrentPrelude
import Peer.Env
import Peer.Event

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Network.Socket
import Reactive.Threepenny

type PeersB = HashMap ByteString PeerBehavior

peerUI :: Behavior PeersB -> UI Element
peerUI behavior = do
    name <- string "Peers"
    peerTable <- sink updatePeersAttr behavior UI.table
    expand name [peerTable]
    where updatePeersAttr = mkWriteAttr updatePeers

updatePeers :: PeersB -> Element -> UI ()
updatePeers ps table = do
    cells <- peers ps
    void (UI.set UI.children cells (UI.element table))

peers :: PeersB -> UI [Element]
peers m = m ^!! ifolded . withIndex . act (uncurry peerInterface)

peerInterface :: ByteString -> PeerBehavior -> UI Element
peerInterface id b = UI.tr #+ [addressColor, interested, choked]
    where   addressColor = sink style (addressStyle <$> remoteChokedB) address
            address = UI.set text (show (b ^. peerAddress)) UI.td
            interested = sink text (showInterested <$> remoteInterestedB) UI.td
            choked = sink text (showChoked <$> remoteChokedB) UI.td
            remoteInterestedB = b ^. remoteStateB . interestedB
            remoteChokedB = b ^. remoteStateB . chokedB

addressStyle :: Bool -> [(String, String)]
addressStyle choked = [("color", chokedColor choked)]

chokedColor :: Bool -> String
chokedColor True = "#BF1616"
chokedColor False = "#00A300"

showInterested :: Bool -> String
showInterested True = "+i"
showInterested False = "-i"

showChoked :: Bool -> String
showChoked True = "+c"
showChoked False = "-c"
