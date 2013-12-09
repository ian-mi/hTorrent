module Peer.Message where

import Common

data MessageType =  Choke |
                    Unchoke |
                    Interested |
                    Uninterested |
                    Have |
                    Bitfield |
                    Request |
                    Piece |
                    Cancel deriving (Enum, Bounded)

minType :: Int
maxType :: Int
minType = fromEnum (minBound :: MessageType)
maxType = fromEnum (maxBound :: MessageType)

data ChunkInd = ChunkInd {  _pieceInd :: Int,
                            _begin :: Int,
                            _length :: Int } deriving (Eq, Ord)

$(makeLenses ''ChunkInd)

data Chunk = Chunk { _chunkInd :: ChunkInd, _chunkData :: ByteString }

$(makeLenses ''Chunk)

data PeerMessage =  ChokeMessage |
                    UnchokeMessage |
                    InterestedMessage |
                    UninterestedMessage |
                    HaveMessage Int |
                    BitfieldMessage IntSet |
                    RequestMessage ChunkInd |
                    PieceMessage Chunk |
                    CancelMessage ChunkInd
