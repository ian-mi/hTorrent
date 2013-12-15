module Torrent.Event where

import HTorrentPrelude
import Peer.State

data TorrentEvent =
    PieceCompleted Int |
    PeerConnected ByteString PeerState
