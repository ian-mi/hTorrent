module Peer.Message.Get (getPacket, getMessage) where

import Data.Chunk
import Peer.Get.Exception
import Peer.Message

import HTorrentPrelude
import Data.Binary.Get
import Data.Bits.Lens
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Data.Interval
import Data.IntSet.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Lens
import qualified Data.ByteString.Lazy as LBS

getPacket :: Get LBS.ByteString
getPacket = getInt >>= getLazyByteString . fromIntegral

getMessage :: Get PeerMessage
getMessage = getWord8 >>= f . fromIntegral
    where f t
            | minType <= t && t <= maxType = getMessageData (toEnum t)
            | otherwise = fail "invalid message type"

getMessageData :: MessageType -> Get PeerMessage
getMessageData Choke = return ChokeMessage
getMessageData Unchoke = return UnchokeMessage
getMessageData Interested = return InterestedMessage
getMessageData Uninterested = return UninterestedMessage
getMessageData Have = HaveMessage <$> getInt
getMessageData Bitfield = BitfieldMessage <$> getBitfield
getMessageData Request = RequestMessage <$> getChunk
getMessageData Piece = do
    p <- getInt
    a <- getInt
    d <- LBS.toStrict <$> getRemainingLazyByteString
    let c = Chunk p (Interval a (a + length d - 1))
    return (PieceMessage c d)
getMessageData Cancel = CancelMessage <$> getChunk

getChunk :: Get Chunk
getChunk = Chunk <$> getInt <*> getInterval

getInterval :: Get Interval
getInterval = do
    a <- getInt
    l <- getInt
    return (Interval a (a + l - 1))

getInt :: Get Int
getInt = fromIntegral <$> getWord32be

toBits :: LBS.ByteString -> IntSet
toBits = setOf (indexing (bytes . backwards bits) . filtered id . asIndex)

getBitfield :: Get IntSet
getBitfield = toBits <$> getRemainingLazyByteString
