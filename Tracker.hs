module Tracker where

import BEncode
import MetaInfo
import Torrent
import Peer
import CompactPeer

import HTorrentPrelude
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as CBS
import Network.HTTP
import Network.HTTP.Types.URI
import Network.Socket
import Network.URI

formatRequest :: TorrentState -> Request ByteString
formatRequest s = mkRequest GET uri
    where   query = renderSimpleQuery True [
                ("info_hash", s ^. metaInfo . info . hash),
                ("peer_id", s ^. peerId),
                ("port", fromString (show (s ^. portNumber))),
                ("uploaded", fromString (show (s ^. uploaded))),
                ("downloaded", fromString (show (s ^. downloaded))),
                ("left", fromString (show (s ^. remaining))),
                ("compact", "1"),
                ("event", "started") ]
            uri = (s ^. metaInfo . announce) {uriQuery = CBS.unpack query}

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
