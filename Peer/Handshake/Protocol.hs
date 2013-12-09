module Peer.Handshake.Protocol where

import HTorrentPrelude
import qualified Data.ByteString as BS

protocol :: ByteString
protocol = "BitTorrent protocol"

protocolLength :: Int
protocolLength = BS.length protocol

reservedBytes :: ByteString
reservedBytes = BS.replicate 8 0
