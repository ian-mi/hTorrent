module Peer.Message.Get (getMessages) where

import Peer.Message

import HTorrentPrelude
import Data.Binary.Get
import Data.Bits.Lens
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Data.IntSet.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Lens
import qualified Data.ByteString.Lazy as LBS

getMessages :: MonadThrow m => Conduit ByteString m PeerMessage
getMessages = conduitGet getPacket =$= CL.map (runGet getMessage)

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
getMessageData Request = RequestMessage <$> getChunkInd
getMessageData Piece = PieceMessage <$> getChunk
getMessageData Cancel = CancelMessage <$> getChunkInd

getChunk :: Get Chunk
getChunk = do   i <- getInt
                b <- getInt
                p <- LBS.toStrict <$> getRemainingLazyByteString
                return (Chunk (ChunkInd i b (BS.length p)) p)

getChunkInd :: Get ChunkInd
getChunkInd = ChunkInd <$> getInt <*> getInt <*> getInt

getInt :: Get Int
getInt = fromIntegral <$> getWord32be

toBits :: LBS.ByteString -> IntSet
toBits = setOf (indexing (bytes . backwards bits) . filtered id . asIndex)

getBitfield :: Get IntSet
getBitfield = toBits <$> getRemainingLazyByteString
