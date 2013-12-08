module Peer.Handshake.Protocol where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS

protocol :: BS.ByteString
protocol = "BitTorrent protocol"

protocolLength :: Int
protocolLength = BS.length protocol

reservedBytes :: BS.ByteString
reservedBytes = BS.pack (replicate 8 0)
