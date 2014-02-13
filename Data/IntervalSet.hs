module Data.IntervalSet where

import Data.Interval
import HTorrentPrelude

import Data.IntMap hiding (union)

newtype IntervalSet = IntervalSet { _intervalSet :: IntMap Int }
$(makeLenses ''IntervalSet)

instance Show IntervalSet where
    show s = show (s ^.. intervals)

intervals :: Fold IntervalSet Interval
intervals = intervalSet . ifolded . withIndex . to (uncurry Interval)

above :: Int -> Fold IntervalSet Interval
above i = strictlyAbove (i - 1)

strictlyAbove :: Int -> Fold IntervalSet Interval
strictlyAbove i = to (i,) . unfolded f
    where f (j, s) = do
            (a, b) <- views intervalSet (lookupGT j) s
            return (Interval a b, (b, s))

notBelow :: Int -> Fold IntervalSet Interval
notBelow i f s
    | Just (a, b) <- views intervalSet (lookupLT i) s, b > i
        = f (Interval a b) *> strictlyAbove b f s
    | otherwise = above i f s

notStrictlyBelow :: Int -> Fold IntervalSet Interval
notStrictlyBelow i f s
    | Just (a, b) <- views intervalSet (lookupLT i) s, b >= i
        = f (Interval a b) *> strictlyAbove b f s
    | otherwise = above i f s

containedBy :: Interval -> Fold IntervalSet Interval
containedBy (Interval a b) = takingWhile (boundedAbove b) (above a)

strictlyContainedBy :: Interval -> Fold IntervalSet Interval
strictlyContainedBy (interior -> Just i) = containedBy i
strictlyContainedBy _ = ignored

containing :: Interval -> IntervalSet -> Maybe Interval
containing (Interval a b) (IntervalSet s)
    | Just (c, d) <- lookupLE a s, d >= b = Just (Interval c d)
    | otherwise = Nothing

intersecting :: Interval -> Fold IntervalSet Interval
intersecting (Interval a b) =
    takingWhile (not . boundedStrictlyBelow b) (notStrictlyBelow a)

touching :: Interval -> Fold IntervalSet Interval
touching (Interval a b) = intersecting (Interval (a - 1) (b + 1))

test :: IntervalSet
test = IntervalSet (mapFromList [(2, 5), (8, 9), (11, 19)])

deleteInterval :: Action (State IntervalSet) Interval ()
deleteInterval = act ((intervalSet %=) . deleteMap . view lower)

insertInterval :: Action (State IntervalSet) Interval ()
insertInterval = act f
    where f (Interval a b) = intervalSet %= insertMap a b

addInterval :: Interval -> IntervalSet -> IntervalSet
addInterval i s = execState (i' ^! insertInterval) s'
    where   is = s ^.. touching i
            i' = foldBy sup i is
            s' = execState (is ^!! folded . deleteInterval) s

removeInterval :: Interval -> MonadicFold (State IntervalSet) IntervalSet Interval
removeInterval i = deleteTouching . insertComplement . remainder
    where   deleteTouching = touching i . pass deleteInterval
            insertComplement = pass (minus i . insertInterval)
            remainder = to (inf i) . _Just

pass :: MonadicFold m a b -> IndexPreservingAction m a a
pass e = act f
    where f a = a ^!! e >> return a
