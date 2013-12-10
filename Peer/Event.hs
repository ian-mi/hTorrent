module Peer.Event (PeerEvent(..)) where

import HTorrentPrelude

data PeerEvent =
    InterestedLocal Bool |
    ChokedLocal Bool |
    InterestedRemote Bool |
    ChokedRemote Bool
