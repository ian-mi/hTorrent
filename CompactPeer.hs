module CompactPeer where

import HTorrentPrelude
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Network.Socket

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32

parseAddress :: Get Word32
parseAddress = htonl <$> getWord32be

parsePort :: Get PortNumber
parsePort = fromIntegral <$> getWord16be

parseCompactPeer :: Get SockAddr
parseCompactPeer = flip SockAddrInet <$> parseAddress <*> parsePort

compactPeers :: ByteString -> Maybe [SockAddr]
compactPeers bs = result ^? _Right . _3
    where result = runGetOrFail (many parseCompactPeer) (LBS.fromStrict bs)
