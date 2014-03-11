module Files where

import HTorrentPrelude
import MetaInfo

import Data.Interval
import Data.Isometry

data FileChunk = FileChunk {
    _chunkPath :: String,
    _chunkRange :: Interval }
$(makeLenses ''FileChunk)

instance Isometry FileChunk where
    len = views chunkRange len
    restrictLower l = chunkRange %~ restrictLower l
    restrictUpper u = chunkRange %~ restrictUpper u

fileChunk :: File -> FileChunk
fileChunk (File l p) = FileChunk p (Interval 0 (l - 1))

fileMap :: [File] -> Piecewise FileChunk
fileMap = fromIsometries . map fileChunk

initFileMap :: Info -> Piecewise FileChunk
initFileMap info = case _file info of
                    Left size -> fileMap [File size (info ^. name)]
                    Right files -> fileMap files
