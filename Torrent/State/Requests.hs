module Torrent.State.Requests where

import HTorrentPrelude
import Data.Chunk
import Data.Interval as I

import Data.IntervalMap as IM
import Data.Monoid

data Requests = Requests {
    _requests :: IntMap (IntervalMap Int [ByteString]) } deriving Show
$(makeLenses ''Requests)

nonEmpty :: Iso' (IntervalMap k a) (Maybe (IntervalMap k a))
nonEmpty = iso f g
    where   f m = if IM.null m then Nothing else Just m
            g (Just m) = m
            g Nothing = IM.empty

pieceRequests :: Int -> Lens' Requests (IntervalMap Int [ByteString])
pieceRequests p = requests . at p . from nonEmpty

requestsWithinChunk :: Chunk -> IndexedFold Chunk Requests ByteString
requestsWithinChunk c =
    reindexed (($ c) . (interval .~) . toInterval) (withinChunk c) <. folded

intervalsWithinChunk :: Chunk -> Fold Requests (IM.Interval Int)
intervalsWithinChunk c = withinChunk c . asIndex

withinChunk :: Chunk -> IndexedFold (IM.Interval Int) Requests [ByteString]
withinChunk (Chunk p i) = requests . at p . _Just .> withinInterval i

withinInterval :: I.Interval -> IndexedFold (IM.Interval Int) (IntervalMap Int a) a
withinInterval = ifolding . flip within . toClosedInterval

emptyRequests :: Requests
emptyRequests = Requests mempty

insertRequest :: Chunk -> ByteString -> Requests -> Requests
insertRequest (Chunk p i) id = pieceRequests p %~ f
    where f = IM.insertWith (++) (toClosedInterval i) [id]

removeRequests :: Chunk -> Requests -> ([(Chunk, ByteString)], Requests)
removeRequests c r = (ids, r')
    where   f = appEndo (foldMapOf (intervalsWithinChunk c) (Endo . IM.delete) r)
            r' = (pieceRequests (c ^. piece) %~ f) r
            ids = r ^@.. requestsWithinChunk c

toClosedInterval :: I.Interval -> IM.Interval Int
toClosedInterval (Interval a b) = ClosedInterval a b

toInterval :: IM.Interval Int -> I.Interval
toInterval (ClosedInterval a b) = Interval a b
toInterval (OpenInterval a b) = Interval (a + 1) (b - 1)
toInterval (IntervalCO a b) = Interval a (b - 1)
toInterval (IntervalOC a b) = Interval (a + 1) b
