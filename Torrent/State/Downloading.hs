module Torrent.State.Downloading where

import Data.Interval
import Data.IntervalSet
import Data.IntervalInverseMap as IIM
import Files
import Files.MMap
import HTorrentPrelude

import Data.Isometry

data PieceState = PieceState {
    _requestedChunks :: IntervalInverseMap,
    _mappedChunks :: Piecewise MappedChunk }
$(makeLenses ''PieceState)

newtype Downloading = Downloading { _downloadingPieces :: IntMap (TVar PieceState) }
$(makeLenses ''Downloading)

nextNeeded :: IntervalSet -> PieceState -> Maybe Interval
nextNeeded ex st = st ^? requestedChunks . inverses . intervals . minusSet ex

request :: Interval -> PieceState -> PieceState
request i = requestedChunks %~ mapInterval i succ

unrequest :: Interval -> PieceState -> PieceState
unrequest i = requestedChunks %~ mapInterval i (subtract 1)

complete :: Interval -> PieceState -> PieceState
complete i = requestedChunks %~ IIM.deleteInterval i

initPieceState :: Piecewise FileChunk -> IO PieceState
initPieceState fs = do
    cs <- mapMOf (piecewise . traverse) mapChunk fs
    return (PieceState (IntervalInverseMap (singletonMap 0 is)) cs)
    where   is = setFromInterval (Interval 0 (l - 1))
            l = len fs

initDownloading :: Int -> Int -> Piecewise FileChunk -> IO Downloading
initDownloading p l fs = do
    ps <- [0..p-1] ^!! folded . to f . act initPieceState . act newTVarIO
    return (Downloading (mapFromList (ps ^@.. ifolded)))
    where f i = restrict (Interval (i * l) (i * l + l - 1)) fs
