module Peer.Handshake.Parse where

import Peer.Handshake.Protocol

import HTorrentPrelude
import Data.Attoparsec as A

parseHandshake :: Parser (ByteString, ByteString)
parseHandshake = do
    word8 (fromIntegral protocolLength) >> string protocol <?> "Protocol"
    A.take 8 <?> "Reserved Bytes"
    (,) <$> (A.take 20 <?> "Hash") <*> (A.take 20 <?> "Peer ID")
