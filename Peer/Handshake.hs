module Peer.Handshake (handshake, HandshakeException) where

import HTorrentPrelude
import MetaInfo
import Peer.Handshake.Conduit
import Torrent.State

import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Network.Socket

data HandshakeException =
    FailedParse ParseError |
    InvalidHash ByteString
    deriving Show

type HandshakeIO a = ExceptionalT HandshakeException IO a

handshake :: TorrentState -> Socket -> HandshakeIO ByteString
handshake t s = do
    lift (runReaderT (sourceHandshake $$ sinkSocket s) t)
    (h, id) <- catchGetHandshake s
    assertT (InvalidHash h) (h == t ^. metaInfo . info . hash)
    return id

getHandshake :: Socket -> IO (ByteString, ByteString)
getHandshake s = sourceSocket s $$ sinkHandshake

catchGetHandshake :: Socket -> HandshakeIO (ByteString, ByteString)
catchGetHandshake s = mapExceptionT FailedParse (catchExplicit (getHandshake s))

catchExplicit :: Exception e => IO a -> ExceptionalT e IO a
catchExplicit m = ExceptionalT (catch (Success <$> m) (return . Exception))
