module Torrent.Event where

import HTorrentPrelude

data TorrentEvent =
    PieceCompleted Int
