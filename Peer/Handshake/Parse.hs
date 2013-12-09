module Peer.Handshake.Parse where

import Common
import Peer.Handshake.Protocol

import Data.Attoparsec as A

parseHandshake :: Parser (ByteString, ByteString)
parseHandshake = do
    word8 (fromIntegral protocolLength) >> string protocol <?> "Protocol"
    A.take 8 <?> "Reserved Bytes"
    (,) <$> (A.take 20 <?> "Hash") <*> (A.take 20 <?> "Peer ID")
