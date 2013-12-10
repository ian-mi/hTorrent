module Interface.Peer (mkPeersBehavior, peerUI, Peers) where

import Interface.Expand
import HTorrentPrelude
import Peer.Env
import Peer.Event

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Network.Socket
import Reactive.Threepenny

data PeerStateB = PeerStateB {
    _interestedB :: Behavior Bool,
    _chokedB :: Behavior Bool
}
$(makeLenses ''PeerStateB)

data PeerBehavior = PeerBehavior {
    _localStateB :: PeerStateB,
    _remoteStateB :: PeerStateB
}
$(makeLenses ''PeerBehavior)

data PeerStateH = PeerStateH {
    _interestedH :: Handler Bool,
    _chokedH :: Handler Bool
}
$(makeLenses ''PeerStateH)

data PeerHandler = PeerHandler {
    _localStateH :: PeerStateH,
    _remoteStateH :: PeerStateH
}
$(makeLenses ''PeerHandler)

type Peers = [(SockAddr, PeerBehavior)]

mkPeersBehavior :: Map SockAddr PeerEnv -> IO (Peers, STM (IO ()))
mkPeersBehavior ps = do
    psBH <- traverse f (mapToList ps)
    let peerBehaviors = over (traversed . _2) fst psBH
    let peerHandler = msumOf (folded . _2 . _2) psBH
    return (peerBehaviors, peerHandler)
    where f = runKleisli (id *** Kleisli mkPeerBehavior)

mkPeerStateB :: ConnectionState -> IO (PeerStateB, PeerStateH)
mkPeerStateB connState = do
    interestedStart <- readTVarIO (connState ^. interested)
    (interestedEvent, interestedHandler) <- newEvent
    interestedBehavior <- stepper interestedStart interestedEvent
    chokedStart <- readTVarIO (connState ^. choked)
    (chokedEvent, chokedHandler) <- newEvent
    chokedBehavior <- stepper chokedStart chokedEvent
    let peerStateBehavior = PeerStateB {
        _interestedB = interestedBehavior,
        _chokedB = chokedBehavior
    }
    let peerStateHandler = PeerStateH {
        _interestedH = interestedHandler,
        _chokedH = chokedHandler
    }
    return (peerStateBehavior, peerStateHandler)

handlePeerEvent :: PeerHandler -> PeerEvent -> IO ()
handlePeerEvent handler event = case event of
    InterestedLocal i -> (handler ^. localStateH . interestedH) i
    ChokedLocal c -> (handler ^. localStateH . chokedH) c
    InterestedRemote i -> (handler ^. remoteStateH . interestedH) i
    ChokedRemote c -> (handler ^. remoteStateH . chokedH) c

handlePeer :: PeerHandler -> TChan (PeerEvent) -> STM (IO ())
handlePeer peerHandler chan = handlePeerEvent peerHandler <$> readTChan chan

mkPeerBehavior :: PeerEnv -> IO (PeerBehavior, STM (IO ()))
mkPeerBehavior peerEnv = do
    chan <- atomically (dupTChan (peerEnv ^. peerEvents))
    (localB, localH) <- mkPeerStateB (peerEnv ^. localState)
    (remoteB, remoteH) <- mkPeerStateB (peerEnv ^. remoteState)
    let peerBehavior = PeerBehavior {
        _localStateB = localB,
        _remoteStateB = remoteB
    }
    let peerHandler = PeerHandler {
        _localStateH = localH,
        _remoteStateH = remoteH
    }
    return (peerBehavior, handlePeer peerHandler chan)

peerUI :: Behavior Peers -> UI Element
peerUI behavior = do
    name <- string "Peers"
    peerTable <- sink updatePeersAttr behavior UI.table
    expand name [peerTable]
    where updatePeersAttr = mkWriteAttr updatePeers

updatePeers :: Peers -> Element -> UI ()
updatePeers ps table = do
    cells <- peers ps
    void (UI.set UI.children cells (UI.element table))

peers :: Peers -> UI [Element]
peers m = mapM (uncurry peerInterface) (mapToList m)

peerInterface :: SockAddr -> PeerBehavior -> UI Element
peerInterface a b = UI.tr #+ [addressColor, interested, choked]
    where   addressColor = sink style (addressStyle <$> remoteChokedB) address
            address = UI.set text (show a) UI.td
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
