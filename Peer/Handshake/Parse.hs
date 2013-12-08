module Peer.Handshake.Parse where

import Peer.Handshake.Protocol

import Control.Applicative
import Data.Attoparsec as A
import qualified Data.ByteString as BS

parseHandshake :: Parser (BS.ByteString, BS.ByteString)
parseHandshake = do
    word8 (fromIntegral protocolLength) >> string protocol <?> "Protocol"
    A.take 8 <?> "Reserved Bytes"
    (,) <$> (A.take 20 <?> "Hash") <*> (A.take 20 <?> "Peer ID")
