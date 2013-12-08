module Tracker where

import BEncode
import MetaInfo
import Torrent
import Peer
import CompactPeer

import Control.Applicative
import Control.Lens
import Data.Attoparsec.ByteString
import Data.ByteString.Char8
import Network.HTTP
import Network.HTTP.Types.URI
import Network.Socket
import Network.URI

formatRequest :: TorrentState -> Request ByteString
formatRequest s = mkRequest GET uri
    where   query = renderSimpleQuery True [
                ("info_hash", s ^. metaInfo . info . hash),
                ("peer_id", s ^. peerId),
                ("port", pack (show (s ^. portNumber))),
                ("uploaded", pack (show (s ^. uploaded))),
                ("downloaded", pack (show (s ^. downloaded))),
                ("left", pack (show (s ^. left))),
                ("compact", "1"),
                ("event", "started") ]
            uri = (s ^. metaInfo . announce) {uriQuery = unpack query}

parseResponse :: BEncode -> Maybe (Int, [Peer])
parseResponse b = (,) <$> i <*> ps
    where   i = bLookup "interval" bInt b
            ps = bLookup "peers" bList b >>= mapM parsePeer

compactResponse :: BEncode -> Maybe (Int, [SockAddr])
compactResponse b = (,) <$> i <*> ps
    where   i = bLookup "interval" bInt b
            ps = bLookup "peers" bString b >>= compactPeers

parseCompactResponse :: ByteString -> Maybe (Int, [SockAddr])
parseCompactResponse = (>>= compactResponse) . maybeResult . parse parseBEncode

getResponse :: TorrentState -> IO ByteString
getResponse ts = simpleHTTP (formatRequest ts) >>= getResponseBody

trackerRequest :: TorrentState -> IO (Maybe (Int, [SockAddr]))
trackerRequest = fmap parseCompactResponse . getResponse
