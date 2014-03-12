module Tracker where

import BEncode
import Torrent.Env
import Peer
import CompactPeer

import HTorrentPrelude
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as CBS
import Network.HTTP
import Network.HTTP.Types.URI
import Network.Socket
import Network.URI

formatRequest :: TorrentInfo -> Request ByteString
formatRequest i = mkRequest GET uri
    where   query = renderSimpleQuery True [
                ("info_hash", i ^. torrentHash),
                ("peer_id", i ^. localId),
                ("port", fromString (show (i ^. portNumber))),
                ("uploaded", fromString (show (i ^. uploaded))),
                ("downloaded", fromString (show 0)),
                ("left", fromString (show (i ^. numPieces * i ^. pieceLength))),
                ("compact", "1"),
                ("event", "started") ]
            uri = (i ^. tracker) {uriQuery = CBS.unpack query}

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

getResponse :: TorrentInfo -> IO ByteString
getResponse i = simpleHTTP (formatRequest i) >>= getResponseBody

trackerRequest :: TorrentInfo -> IO (Maybe (Int, [SockAddr]))
trackerRequest = fmap parseCompactResponse . getResponse
