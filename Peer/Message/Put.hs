module Peer.Message.Put where

import Common
import Peer.Message

import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import qualified Data.IntSet as IS
import Data.Monoid

putMessages :: MonadThrow m => Int -> Conduit PeerMessage m ByteString
putMessages p = CL.map (addLength . putMessage p) =$= conduitPut

addLength :: Put -> Put
addLength p = putInt (fromIntegral (LBS.length (runPut p))) >> p

putMessage :: Int -> PeerMessage -> Put
putMessage _ ChokeMessage = putMessageType Choke
putMessage _ UnchokeMessage = putMessageType Unchoke
putMessage _ InterestedMessage = putMessageType Interested
putMessage _ UninterestedMessage = putMessageType Uninterested
putMessage _ (HaveMessage i) = putMessageType Have >> putInt i
putMessage p (BitfieldMessage b) = putMessageType Bitfield >> putBitfield p b
putMessage _ (RequestMessage i) = putMessageType Request >> putChunkInd i
putMessage _ (PieceMessage (Chunk (ChunkInd p i _) d)) = do
    putMessageType Piece
    putInt p
    putInt i
    putByteString d
putMessage _ (CancelMessage i) = putMessageType Cancel >> putChunkInd i

putMessageType :: MessageType -> Put
putMessageType = putWord8 . fromIntegral . fromEnum

putChunkInd :: ChunkInd -> Put
putChunkInd (ChunkInd p i l) = putInt p >> putInt i >> putInt l

putBitfield :: Int -> IntSet -> Put
putBitfield p b = mapM_ (putWord8 . fromBits) bits
    where   bits = map reverse (groupBits 8 p (IS.toAscList b))

groupBits :: Int -> Int -> [Int] -> [[Int]]
groupBits inc end xs = unfoldr f (0, xs)
    where   f (i, xs) = over _2 (i' ,) (span (< i') xs) <$ guard (i < end)
                where i' = i + inc

fromBits :: [Int] -> Word8
fromBits = (flip appEndo 0) . foldMap (Endo . flip setBit)

putInt :: Int -> Put
putInt = putWord32be . fromIntegral 
