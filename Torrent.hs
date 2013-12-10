module Torrent where

import MetaInfo
import Peer.Connection
import Peer.Env
import Piece
import Tracker
import Torrent.Env
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
    conns <- newTVarIO (mapFromList [])
    return (TorrentState m (BS.pack id) p 0 0 l pn pa env conns)
    where   ps = splitPieces (m ^. info . pieceHashes)
            pn = length ps
            pa = pieceArray pn ps
            l = pn * m ^. info . piece_length

startTorrent :: MetaInfo -> PortNumber -> IO (Maybe TorrentState)
startTorrent m p = do
    ts <- initTorrent m p
    tr <- trackerRequest ts
    case tr of 
        Just (_, as) -> do
            connectPeers ts as >>= atomically . writeTVar (ts ^. peers)
            return (Just ts)
        Nothing -> return Nothing

connectPeers :: TorrentState -> [SockAddr] -> IO (Map SockAddr PeerEnv)
connectPeers ts as = mapFromList <$> mapM f as
    where f = runKleisli (id &&& Kleisli (forkPeer ts))
