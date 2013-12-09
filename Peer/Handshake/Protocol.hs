module Peer.Handshake.Protocol where

import Common

import qualified Data.ByteString as BS

protocol :: ByteString
protocol = "BitTorrent protocol"

protocolLength :: Int
protocolLength = BS.length protocol

reservedBytes :: ByteString
reservedBytes = BS.replicate 8 0
