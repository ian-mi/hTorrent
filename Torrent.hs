module Torrent where

import Common
import MetaInfo
import Piece
import Torrent.Env

import Data.Array
import qualified Data.ByteString as BS
import Network
import System.Random

data TorrentState = TorrentState {  _metaInfo :: MetaInfo, 
                                    _peerId :: ByteString,
                                    _portNumber :: PortNumber,
                                    _uploaded :: Int,
                                    _downloaded :: Int,
                                    _remaining :: Int,
                                    _numPieces :: Int,
                                    _pHashes :: Array Int ByteString,
                                    _env :: TorrentEnv
                                    }

$(makeLenses ''TorrentState)

splitPieces :: ByteString -> [ByteString]
splitPieces = unfoldr f
    where f bs = guard (not (BS.null bs)) >> Just (BS.splitAt 20 bs)

pieceArray :: Int -> [ByteString] -> Array Int ByteString
pieceArray n bs = listArray (1, n) bs

initTorrent :: MetaInfo -> PortNumber -> IO TorrentState
initTorrent m p = do
    id <- replicateM 20 randomIO
    env <- initTorrentEnv pn (m ^. (info . piece_length))
    return (TorrentState m (BS.pack id) p 0 0 l pn pa env)
    where   ps = splitPieces (m ^. info . pieces)
            pn = length ps
            pa = pieceArray pn ps
            l = pn * m ^. info . piece_length
