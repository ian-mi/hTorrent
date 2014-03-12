module Peer.Handshake (handshake, HandshakeExcept(..)) where

import HTorrentPrelude
import Peer.Env
import Torrent.Event
import Peer.Handshake.Conduit
import Torrent.Env

import Control.Concurrent.Async
import qualified Control.Monad.Exception.Synchronous as E
import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Network.Socket

data HandshakeExcept =
    FailedParse ParseError |
    InvalidHash ByteString |
    InvalidId ByteString
    deriving Show

handshake :: Socket -> TorrentInfo -> ExceptT HandshakeExcept IO ByteString
handshake s info = do
    (h, id) <- E.ExceptionalT (snd <$> concurrently sendId (E.tryT getId))
    id <$ assert (InvalidHash h) (h == info ^. torrentHash)
    where   sendId = runReaderT (sourceHandshake $$ sinkSocket s) info
            getId = mapExceptT FailedParse (getHandshake s)

getHandshake :: Socket -> ExceptT ParseError IO (ByteString, ByteString)
getHandshake s = except (sourceSocket s $$ sinkHandshake)
