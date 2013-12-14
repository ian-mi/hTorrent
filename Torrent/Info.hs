module Torrent.Info where

import HTorrentPrelude

data TorrentInfo = TorrentInfo {
    _torrentName :: String,
    _numPieces :: Int
}
$(makeLenses ''TorrentInfo)
