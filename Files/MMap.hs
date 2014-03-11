module Files.MMap where

import Files
import HTorrentPrelude

import Data.ByteString
import Data.ByteString.Unsafe
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.IntMap
import Data.Interval
import Data.Isometry
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.MMap

data MappedChunk = MappedChunk {
    _mappedMem :: ForeignPtr CChar,
    _mappedChunkOffset :: Int,
    _mappedChunkSize :: Int
}
$(makeLenses ''MappedChunk)

instance Isometry MappedChunk where
    len = _mappedChunkSize
    restrictLower l c = (mappedChunkSize -~ l') ((mappedChunkOffset +~ l') c)
        where l' = max 0 (min (c ^. mappedChunkSize) l)
    restrictUpper u = mappedChunkSize %~ max 0 . min (u + 1)

mapChunk :: FileChunk -> IO MappedChunk
mapChunk (FileChunk path (Interval a b)) = do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadWriteEx (Just (s, l))
    return (MappedChunk ptr offset size)
    where   s = fromIntegral a
            l = b - a + 1

mappedChunksSink :: Piecewise MappedChunk -> Sink ByteString IO ()
mappedChunksSink = mapMOf_ (piecewise . folded) mappedChunkSink

mappedChunkSink :: MappedChunk -> Sink ByteString IO ()
mappedChunkSink (MappedChunk ptr offset size) = isolate size =$ ptrSink ptr offset

ptrSink :: ForeignPtr CChar -> Int -> Sink ByteString IO ()
ptrSink ptr = void . CL.foldM f
    where   f offset bs = unsafeUseAsCStringLen bs (g offset)
            g offset (cstr, len) = do
                withForeignPtr ptr (\p -> copyBytes (plusPtr p offset) cstr len)
                return (offset + len)

mappedChunksSource :: Piecewise MappedChunk -> Source IO ByteString
mappedChunksSource = mapMOf_ (piecewise . folded) mappedChunkSource

mappedChunkSource :: MappedChunk -> Source IO ByteString
mappedChunkSource c = lift (readChunk c) >>= yield

readChunk :: MappedChunk -> IO ByteString
readChunk (MappedChunk ptr off ln) = withForeignPtr ptr f
    where f p = packCStringLen (plusPtr p off, ln)
