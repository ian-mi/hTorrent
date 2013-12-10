module Torrent.Env where

import Piece
import Torrent.Event

import HTorrentPrelude
import qualified Data.IntMap as IM

data TorrentEnv = TorrentEnv {
    _completed :: TVar (IntMap ByteString),
    _downloading :: TVar (IntMap (TVar PieceBuffer)),
    _torrentEvents :: TChan TorrentEvent
}
$(makeLenses ''TorrentEnv)

initTorrentEnv :: Int -> Int -> IO (TorrentEnv)
initTorrentEnv n l = do
    c <- newTVarIO IM.empty
    d <- newDownloading n l
    events <- newBroadcastTChanIO
    return TorrentEnv {
        _completed = c,
        _downloading = d,
        _torrentEvents = events
    }

newDownloading :: Int -> Int -> IO (TVar (IntMap (TVar PieceBuffer)))
newDownloading s p = do
    mapM f [0..s-1] >>= newTVarIO . IM.fromList
    where f i = (i,) <$> newTVarIO (emptyBuffer p)
