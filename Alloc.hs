module Alloc where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.IntMap as IM
import Data.Maybe

class Alloc a where
    size :: a -> Int
    truncate :: Int -> a -> a
    delete :: Int -> a -> a
    append :: a -> a -> a

merge :: Alloc a => Int -> a -> a -> a
merge o a b = append a (delete o b)

instance Alloc Int where
    size = id
    truncate = min . max 0
    delete d a = max 0 (a - max 0 d)
    append = (+)

instance Alloc BS.ByteString where
    size = BS.length
    append = BS.append
    delete = BS.drop
    truncate = BS.take

complementAsc :: Int -> [(Int, Int)] -> [(Int, Int)]
complementAsc s is = filter (not . uncurry (==)) c
    where   c = zipWith (,) (0 : b) (t ++ [s])
            t = is ^.. traverse . _1
            b = is ^.. traverse . _2

freeAsc :: Alloc a => Int -> IM.IntMap a -> [(Int,Int)]
freeAsc s = complementAsc s . usedAsc

usedAsc :: Alloc a => IM.IntMap a -> [(Int,Int)]
usedAsc = fmap f . IM.toAscList
    where f (s, l) = (s, s + size l)

unionAlloc :: Alloc a => IM.IntMap a -> IM.IntMap a -> IM.IntMap a
unionAlloc = ifoldrOf itraversed (curry (execState . alloc))

alloc :: (Alloc a, MonadState (IM.IntMap a) m) => (Int, a) -> m ()
alloc n@(i, _) = do
    m <- gets (fromMaybe n . (>>= flip mergeAlloc n) . IM.lookupLE i)
    u <- gets (IM.lookupGE i)
    case u >>= runKleisli (arr fst &&& Kleisli (mergeAlloc m)) of
        Just (j, m') -> modify (IM.delete j) >> modify (uncurry IM.insert m')
        Nothing -> modify (uncurry IM.insert m)

mergeAlloc :: Alloc a => (Int, a) -> (Int, a) -> Maybe (Int, a)
mergeAlloc (i, x) (j, y)
    | overlap >= 0  = Just (i, merge overlap x y)
    | otherwise     = Nothing
    where   overlap = i + size x - j
