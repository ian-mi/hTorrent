module Data.Chunk where

import HTorrentPrelude

import Data.Interval

data Chunk = Chunk {
    _piece :: Int,
    _interval :: Interval
    } deriving (Eq, Ord, Show)

$(makeLenses ''Chunk)
