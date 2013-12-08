module Peer where

import BEncode

import Control.Applicative
import Data.ByteString.UTF8
import Network

data Peer = Peer {  _id :: ByteString,
                    _address :: String,
                    _port :: PortNumber } deriving Show

parsePeer :: BEncode -> Maybe Peer
parsePeer b = Peer <$> i <*> a <*> p
    where   i = bLookup "peer id" bString b
            a = toString <$> bLookup "ip" bString b
            p = fromIntegral <$> bLookup "port" bInt b
