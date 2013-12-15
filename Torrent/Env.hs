module Torrent.Env where

import Peer.State
import Piece
import Torrent.Event

import Data.Array
import HTorrentPrelude
import qualified Data.IntMap as IM
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
    _pieceHash :: Array Int ByteString
}
$(makeLenses ''TorrentInfo)

data TorrentEnv = TorrentEnv {
    _torrentInfo :: TorrentInfo,
    _completed :: TVar (IntMap ByteString),
    _numCompleted :: TVar Int,
    _downloading :: TVar (IntMap (TVar PieceBuffer)),
    _peers :: TVar (HashMap ByteString PeerState),
    _torrentEvents :: TChan TorrentEvent
}
$(makeLenses ''TorrentEnv)

initTorrentEnv :: TorrentInfo -> IO TorrentEnv
initTorrentEnv i = do
    c <- newTVarIO IM.empty
    nC <- newTVarIO 0
    d <- newDownloading i
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

newDownloading :: TorrentInfo -> IO (TVar (IntMap (TVar PieceBuffer)))
newDownloading info = do
    mapM f [0 .. info ^. numPieces - 1] >>= newTVarIO . IM.fromList
    where   f i = (i,) <$> newTVarIO (emptyBuffer (info ^. pieceLength))
            n = info ^. numPieces
