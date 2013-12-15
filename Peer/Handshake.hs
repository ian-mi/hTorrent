module Peer.Handshake (handshake, HandshakeExcept) where

import HTorrentPrelude
import Peer.Env
import Torrent.Event
import Peer.Handshake.Conduit
import Torrent.Env

import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Network.Socket

data HandshakeExcept =
    FailedParse ParseError |
    InvalidHash ByteString |
    InvalidId ByteString
    deriving Show

handshake :: Socket -> ExceptT HandshakeExcept (ReaderT PeerEnv IO) ()
handshake s = do
    lift (magnify (torrentEnv . torrentInfo) (sourceHandshake $$ sinkSocket s))
    (h, id) <- hoist lift (mapExceptT FailedParse (getHandshake s))
    views (torrentEnv . torrentInfo . torrentHash) (h ==) >>= assert (InvalidHash h)
    addPeer id

addPeer :: ByteString -> ExceptT HandshakeExcept (ReaderT PeerEnv IO) ()
addPeer id = hoist (hoist atomically) $ do
    ps <- viewTVar (torrentEnv . peers)
    assert (InvalidId id) (not (member id ps))
    st <- view peerState
    torrentEnv . peers &.= insertMap id st ps
    torrentEnv . torrentEvents &-< PeerConnected id st

getHandshake :: Socket -> ExceptT ParseError IO (ByteString, ByteString)
getHandshake s = except (sourceSocket s $$ sinkHandshake)
