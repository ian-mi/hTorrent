module Torrent.Env where

import Files
import Files.MMap
import MetaInfo
import Peer.State
import Torrent.Event
import Torrent.State.Downloading

import Data.Array
import HTorrentPrelude
import qualified Data.IntMap as IM
import Data.Isometry
import Network.URI
import Network.Socket

data TorrentInfo = TorrentInfo {
    _torrentName :: String,
    _torrentHash :: ByteString,
    _tracker :: URI,
    _numPieces :: Int,
    _pieceLength :: Int,
    _peerId :: ByteString,
    _portNumber :: PortNumber,
    _uploaded :: Int,
    _pieceHash :: Array Int ByteString,
    _files :: Piecewise FileChunk
}
$(makeLenses ''TorrentInfo)

data TorrentEnv = TorrentEnv {
    _torrentInfo :: TorrentInfo,
    _completed :: TVar (IntMap ByteString),
    _numCompleted :: TVar Int,
    _downloading :: TVar Downloading,
    _peers :: TVar (HashMap ByteString PeerState),
    _torrentEvents :: TChan TorrentEvent
}
$(makeLenses ''TorrentEnv)

initTorrentEnv :: TorrentInfo -> IO TorrentEnv
initTorrentEnv i = do
    c <- newTVarIO mempty
    nC <- newTVarIO 0
    d <- initDownloading (i ^. numPieces) (i ^. pieceLength) (i ^. files) >>= newTVarIO
    ps <- newTVarIO mempty
    events <- newBroadcastTChanIO
    return TorrentEnv {
        _torrentInfo = i,
        _completed = c,
        _numCompleted = nC,
        _downloading = d,
        _peers = ps,
        _torrentEvents = events
    }
