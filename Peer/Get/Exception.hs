module Peer.Get.Exception where

import HTorrentPrelude

import Data.Conduit.Serialization.Binary

data PeerGetE = IncompletePacket ParseError |
                InvalidMessage ParseErrorLazy deriving Show
