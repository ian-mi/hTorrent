module Main where

import Interface
import MetaInfo
import Torrent

import HTorrentPrelude
import Network.HTTP
import Network.Socket

main :: IO ()
main = void (runMaybeT (loadTorrent filePath))
    where filePath = "debian-7.2.0-amd64-CD-1.iso.torrent"

loadTorrent :: String -> MaybeT IO ()
loadTorrent filePath = do
    metaInfo <- MaybeT (readTorrent filePath)
    torrentState <- MaybeT (startTorrent metaInfo 50000)
    lift (startInterface torrentState)
