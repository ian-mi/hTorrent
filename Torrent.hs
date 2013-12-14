module Torrent where

import MetaInfo
import Peer.Connection
import Peer.Env
import Piece
import Tracker
import Torrent.Env
import Torrent.Info
import Torrent.State

import HTorrentPrelude
import Data.Array
import qualified Data.ByteString as BS
import Network.Socket
import System.Random

splitPieces :: ByteString -> [ByteString]
splitPieces = unfoldr f
    where f bs = guard (not (BS.null bs)) >> Just (BS.splitAt 20 bs)

pieceArray :: Int -> [ByteString] -> Array Int ByteString
pieceArray n bs = listArray (1, n) bs

initTorrent :: MetaInfo -> PortNumber -> IO TorrentState
initTorrent m p = do
    id <- replicateM 20 randomIO
    env <- initTorrentEnv pn (m ^. (info . piece_length))
    return (TorrentState i m (BS.pack id) p 0 0 l pa env)
    where   ps = splitPieces (m ^. info . pieceHashes)
            pn = length ps
            pa = pieceArray pn ps
            l = pn * m ^. info . piece_length
            i = TorrentInfo {_torrentName = m ^. info . name, _numPieces = pn}

startTorrent :: MetaInfo -> PortNumber -> IO (Maybe TorrentState)
startTorrent m p = do
    ts <- initTorrent m p
    tr <- trackerRequest ts
    case tr of 
        Just (_, as) -> do
            mapM (forkIO . peerThread ts) as
            return (Just ts)
        Nothing -> return Nothing
