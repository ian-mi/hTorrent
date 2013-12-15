module Peer.Handshake.Put where

import MetaInfo
import Peer.Handshake.Protocol
import Torrent.Env

import HTorrentPrelude
import Data.Binary.Put

putHandshake :: ReaderT TorrentInfo PutM ()
putHandshake = do 
    lift putProtocol
    lift (putByteString reservedBytes)
    view torrentHash >>= lift . putByteString
    view peerId >>= lift . putByteString

putProtocol :: Put
putProtocol = putWord8 (fromIntegral protocolLength) >> putByteString protocol
