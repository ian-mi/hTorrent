module Main where

import CompactPeer
import Interface
import MetaInfo
import Torrent
import Tracker
import Peer.Connection

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Network.HTTP
import Network.Socket

main :: IO ()
main = do
    m <- readTorrent "debian-7.2.0-amd64-CD-1.iso.torrent"
    maybe (putStr "invalid torrent") startTorrent m

startTorrent :: MetaInfo -> IO ()
startTorrent metaInfo = do
    ts <- initTorrent metaInfo 50000
    m <- trackerRequest ts
    runReaderT (maybe (return ()) (connectPeers . snd) m) ts

connectPeers :: [SockAddr] -> ReaderT TorrentState IO ()
connectPeers ps = mapM_ forkPeer ps >> ReaderT startInterface
