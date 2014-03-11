module Peer.Get.Packet (messageConduit) where

import HTorrentPrelude
import Peer.Get.Exception
import Peer.Message
import Peer.Message.Get

import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Serialization.Binary
import qualified Data.Conduit.List as CL
import Control.Monad.Exception.Synchronous

messageConduit :: Conduit ByteString (ExceptT PeerGetE IO) PeerMessage
messageConduit = packetConduit =$= CL.mapM (ExceptionalT . return . parsePacket)

packetConduit :: Conduit ByteString (ExceptT PeerGetE IO) LBS.ByteString
packetConduit = hoist (mapExceptT IncompletePacket . except) (conduitGet getPacket)

parsePacket :: LBS.ByteString -> Exceptional PeerGetE PeerMessage
parsePacket bs = mapException InvalidMessage (runGetExcept getMessage bs)
