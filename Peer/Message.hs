module Peer.Message where

import HTorrentPrelude
import Data.Chunk

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

data PeerMessage =  ChokeMessage |
                    UnchokeMessage |
                    InterestedMessage |
                    UninterestedMessage |
                    HaveMessage Int |
                    BitfieldMessage IntSet |
                    RequestMessage Chunk |
                    PieceMessage Chunk ByteString |
                    CancelMessage Chunk
