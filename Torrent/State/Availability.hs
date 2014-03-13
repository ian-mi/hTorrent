module Torrent.State.Availability where

import HTorrentPrelude

import Control.Concurrent.STM.TArray
import Data.Array.MArray
import Data.IntSet.Lens

newtype Availability = Availability { _availabilityArray :: TArray Int Int }
$(makeLenses ''Availability)

incAvail :: Int -> ReaderT Availability STM ()
incAvail p = do
    av <- view availabilityArray
    lift (readArray av p >>= writeArray av p . succ)

decAvail :: Int -> ReaderT Availability STM ()
decAvail p = do
    av <- view availabilityArray
    lift (readArray av p >>= writeArray av p . (subtract 1))

initAvail :: Int -> STM Availability
initAvail p = Availability <$> newArray (0, p - 1) 0

rarest :: IntSet -> Availability -> STM [Int]
rarest ps (Availability a) = do
    as <- ps ^@!! members .> iact (\p -> (,p) <$> readArray a p)
    case minimumOf (folded . _1) as of
        Just n -> return (as ^.. ifolding id . ifiltered (const . (== n)))
        Nothing -> return []
