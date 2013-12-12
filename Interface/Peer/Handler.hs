module Interface.Peer.Handler where

import HTorrentPrelude
import Peer.Event

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

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

data PeerHandlerEnv = PeerHandlerEnv {
    _peerHandler :: PeerHandler,
    _peerChan :: TChan PeerEvent
}

handlePeer :: PeerHandlerEnv -> IO ()
handlePeer (PeerHandlerEnv handler chan) = forever $ do
    event <- atomically (readTChan chan)
    handlePeerEvent handler event

handlePeerEvent :: PeerHandler -> PeerEvent -> IO ()
handlePeerEvent handler event = case event of
    InterestedLocal i -> (handler ^. localStateH . interestedH) i
    ChokedLocal c -> (handler ^. localStateH . chokedH) c
    InterestedRemote i -> (handler ^. remoteStateH . interestedH) i
    ChokedRemote c -> (handler ^. remoteStateH . chokedH) c
