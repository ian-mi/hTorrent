module Peer.Handshake (handshake, HandshakeException) where

import HTorrentPrelude
import MetaInfo
import Peer.Env
import Torrent.Event
import Peer.Handshake.Conduit
import Torrent.Env
import Torrent.State

import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Network.Socket

data HandshakeException =
    FailedParse ParseError |
    InvalidHash ByteString |
    InvalidId ByteString
    deriving Show

type HandshakeIO a = ExceptionalT HandshakeException IO a
type HandshakeSTM a = ExceptionalT HandshakeException STM a

handshake :: TorrentState -> Socket -> HandshakeIO PeerEnv
handshake t s = do
    lift (runReaderT (sourceHandshake $$ sinkSocket s) t)
    (h, id) <- catchGetHandshake s
    assertT (InvalidHash h) (h == t ^. metaInfo . info . hash)
    peerEnv <- lift initPeerEnv
    mapExceptionalT atomically (addPeer (t ^. env) id peerEnv)
    lift (atomically (writeTChan (t ^. env . torrentEvents) (PeerConnected id peerEnv)))
    return peerEnv

addPeer :: TorrentEnv -> ByteString -> PeerEnv -> HandshakeSTM ()
addPeer tEnv id env = do
    ps <- liftSTM (readTVar (tEnv ^. peers))
    assertT (InvalidId id) (not (member id ps))
    liftSTM (writeTVar (tEnv ^. peers) (insertMap id env ps))

getHandshake :: Socket -> IO (ByteString, ByteString)
getHandshake s = sourceSocket s $$ sinkHandshake

catchGetHandshake :: Socket -> HandshakeIO (ByteString, ByteString)
catchGetHandshake s = mapExceptionT FailedParse (catchExplicit (getHandshake s))

catchExplicit :: Exception e => IO a -> ExceptionalT e IO a
catchExplicit m = ExceptionalT (catch (Success <$> m) (return . Exception))
