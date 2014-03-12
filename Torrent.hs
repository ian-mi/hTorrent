module Torrent where

import Files
import Files.MMap
import MetaInfo
import Peer.Connection
import Peer.Env
import Tracker
import Torrent.Env

import HTorrentPrelude
import Data.Array
import qualified Data.ByteString as BS
import Network.Socket
import System.Random

splitPieces :: ByteString -> [ByteString]
splitPieces = unfoldr f
    where f bs = guard (not (BS.null bs)) >> Just (BS.splitAt 20 bs)

pieceArray :: Int -> [ByteString] -> Array Int ByteString
pieceArray n bs = listArray (0, n - 1) bs

initTorrent :: MetaInfo -> PortNumber -> IO TorrentEnv
initTorrent m p = do
    id <- replicateM 20 randomIO
    let fs = initFileMap (m ^. info)
    let torrentInfo = TorrentInfo {
        _torrentName = m ^. info . name,
        _torrentHash = m ^. info . hash,
        _tracker = m ^. announce,
        _numPieces = pn,
        _pieceLength = m ^. info . piece_length,
        _localId = BS.pack id,
        _portNumber = p,
        _uploaded = 0,
        _pieceHash = pa,
        _files = fs
    }
    initTorrentEnv torrentInfo
    where   ps = splitPieces (m ^. info . pieceHashes)
            pn = length ps
            pa = pieceArray pn ps

startTorrent :: MetaInfo -> PortNumber -> IO (Maybe TorrentEnv)
startTorrent m p = do
    env <- initTorrent m p
    tr <- trackerRequest (env ^. torrentInfo)
    case tr of 
        Just (_, as) -> do
            mapM (forkIO . peerThread env) as
            return (Just env)
        Nothing -> return Nothing
