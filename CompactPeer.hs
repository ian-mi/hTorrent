module CompactPeer where

import Control.Applicative
import Data.Binary.Get
import Data.ByteString as BS
import Data.ByteString.Lazy
import Data.Word
import Control.Lens
import Network.Socket

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32

parseAddress :: Get Word32
parseAddress = htonl <$> getWord32be

parsePort :: Get PortNumber
parsePort = fromIntegral <$> getWord16be

parseCompactPeer :: Get SockAddr
parseCompactPeer = flip SockAddrInet <$> parseAddress <*> parsePort

compactPeers :: BS.ByteString -> Maybe [SockAddr]
compactPeers bs = result ^? _Right . _3
    where result = runGetOrFail (many parseCompactPeer) (fromStrict bs)

