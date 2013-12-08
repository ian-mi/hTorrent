module Peer.Handshake.Put where

import MetaInfo
import Peer.Handshake.Protocol
import Torrent

import Control.Lens
import Control.Monad.Reader
import Data.Binary.Put
import qualified Data.ByteString as BS

putHandshake :: ReaderT TorrentState PutM ()
putHandshake = do 
    lift putProtocol
    lift (putByteString reservedBytes)
    view (metaInfo . info . hash) >>= lift . putByteString
    view peerId >>= lift . putByteString

putProtocol :: Put
putProtocol = putWord8 (fromIntegral protocolLength) >> putByteString protocol
