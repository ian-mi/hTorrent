module Peer.Env where

import HTorrentPrelude
import Peer.Event
import Peer.Message
import Peer.State
import Torrent.Env

import qualified Data.IntSet as IS
import qualified Data.Set as S
import Network.Socket

data PeerEnv = PeerEnv {
    _torrentEnv :: TorrentEnv,
    _peerState :: PeerState
}
$(makeLenses ''PeerEnv)
