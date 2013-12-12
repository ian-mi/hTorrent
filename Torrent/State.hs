module Torrent.State where

import Data.Array
import Network.Socket

import HTorrentPrelude
import MetaInfo
import Peer.Env
import Torrent.Env

data TorrentState = TorrentState {
    _metaInfo :: MetaInfo, 
    _peerId :: ByteString,
    _portNumber :: PortNumber,
    _uploaded :: Int,
    _downloaded :: Int,
    _remaining :: Int,
    _numPieces :: Int,
    _pHashes :: Array Int ByteString,
    _env :: TorrentEnv }

$(makeLenses ''TorrentState)
