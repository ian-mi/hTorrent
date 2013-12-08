module Peer.Handshake.Conduit where

import Peer.Handshake.Parse
import Peer.Handshake.Put
import Torrent

import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Serialization.Binary

sourceHandshake :: (MonadReader TorrentState m, MonadThrow m) =>
    Producer m BS.ByteString
sourceHandshake = ask >>= sourcePut . runReaderT putHandshake

sinkHandshake :: MonadThrow m =>
    Consumer BS.ByteString m (BS.ByteString, BS.ByteString)
sinkHandshake = sinkParser parseHandshake
