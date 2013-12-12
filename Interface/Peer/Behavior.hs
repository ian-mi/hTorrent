module Interface.Peer.Behavior where

import Interface.Peer.Handler
import HTorrentPrelude
import Peer.Env

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
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

mkPeersBehavior :: HashMap ByteString PeerEnv ->
    IO (HashMap ByteString PeerBehavior, HashMap ByteString PeerHandlerEnv)
mkPeersBehavior ps = do
    peerBH <- mapMOf traverse mkPeerBehavior ps
    return (fst <$> peerBH, snd <$> peerBH)

mkPeerBehavior :: PeerEnv -> IO (PeerBehavior, PeerHandlerEnv)
mkPeerBehavior peerEnv = do
    chan <- atomically (dupTChan (peerEnv ^. peerEvents))
    (localB, localH) <- mkPeerStateB (peerEnv ^. localState)
    (remoteB, remoteH) <- mkPeerStateB (peerEnv ^. remoteState)
    let peerB = PeerBehavior {
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
