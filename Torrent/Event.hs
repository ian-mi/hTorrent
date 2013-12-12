module Torrent.Event where

import HTorrentPrelude
import Peer.Env
import Peer.Event

data TorrentEvent =
    PieceCompleted Int |
    PeerConnected ByteString PeerEnv
