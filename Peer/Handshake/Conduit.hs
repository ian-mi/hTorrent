module Peer.Handshake.Conduit where

import Peer.Handshake.Parse
import Peer.Handshake.Put
import Torrent.Env

import HTorrentPrelude
import Data.Conduit.Attoparsec
import Data.Conduit.Serialization.Binary

sourceHandshake :: (MonadReader TorrentInfo m, MonadThrow m) =>
    Producer m ByteString
sourceHandshake = ask >>= sourcePut . runReaderT putHandshake

sinkHandshake :: MonadThrow m =>
    Consumer ByteString m (ByteString, ByteString)
sinkHandshake = sinkParser parseHandshake
