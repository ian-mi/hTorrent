module Piece(PieceBuffer, addData, allocNext, full, emptyBuffer, complete) where

import Alloc
import Common
import Morphisms

import qualified Data.IntMap.Strict as IM
import qualified Data.ByteString as BS

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Lens
import Data.Maybe

data PieceBuffer = PieceBuffer {    _pieceSize :: Int,
                                    _chunks :: IntMap ByteString,
                                    _requested :: IntMap Int }
$(makeLenses ''PieceBuffer)

emptyBuffer :: Int -> PieceBuffer
emptyBuffer s = PieceBuffer s IM.empty IM.empty

addData :: MonadState PieceBuffer m => (Int, ByteString) -> m ()
addData = zoomState chunks . alloc

allocNext :: MonadState PieceBuffer m => m (Maybe (Int, Int))
allocNext = do
    s <- use pieceSize
    zoomState requested (gets (listToMaybe . freeAsc s) >>= traverse (g . f))
    where   f (i, j) = (i, min (j - i) l)
            g r = r <$ alloc r
            l = 2^14

full :: PieceBuffer -> Bool
full b = null (freeAsc (b ^. pieceSize) (b ^. requested))

complete :: PieceBuffer -> Maybe ByteString
complete (PieceBuffer s cs _) = do
    (i, buf) <- IM.lookupLE 0 cs
    BS.take s (BS.drop (-i) buf) <$ guard (i + BS.length buf >= s)
