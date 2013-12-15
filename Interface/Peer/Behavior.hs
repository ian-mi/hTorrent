module Interface.Peer.Behavior where

import Interface.Peer.Handler
import HTorrentPrelude
import Peer.State

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
    _peerAddress :: SockAddr,
    _localStateB :: PeerStateB,
    _remoteStateB :: PeerStateB
}
$(makeLenses ''PeerBehavior)

mkPeersBehavior :: HashMap ByteString PeerState ->
    IO (HashMap ByteString PeerBehavior, HashMap ByteString PeerHandlerEnv)
mkPeersBehavior ps = do
    peerBH <- mapMOf traverse mkPeerBehavior ps
    return (fst <$> peerBH, snd <$> peerBH)

mkPeerBehavior :: PeerState -> IO (PeerBehavior, PeerHandlerEnv)
mkPeerBehavior s = do
    chan <- atomically (dupTChan (s ^. events))
    (localB, localH) <- mkPeerStateB (s ^. localState)
    (remoteB, remoteH) <- mkPeerStateB (s ^. remoteState)
    let peerB = PeerBehavior {
        _peerAddress = s ^. address,
        _localStateB = localB,
        _remoteStateB = remoteB
    }
    let peerH = PeerHandler {
        _localStateH = localH,
        _remoteStateH = remoteH
    }
    let peerHandlerEnv = PeerHandlerEnv {
        _peerHandler = peerH,
        _peerChan = chan
    }
    return (peerB, peerHandlerEnv)

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
