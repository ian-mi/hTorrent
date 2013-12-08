module Torrent.Env where

import Piece

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.IntMap as IM

data TorrentEnv = TorrentEnv {
    _completed :: TVar (IM.IntMap BS.ByteString),
    _completedSig :: TMVar (),
    _downloading :: TVar (IM.IntMap (TVar PieceBuffer))
}
$(makeLenses ''TorrentEnv)

initTorrentEnv :: Int -> Int -> IO (TorrentEnv)
initTorrentEnv n l = do
    c <- newTVarIO IM.empty
    cSig <- newEmptyTMVarIO
    d <- newDownloading n l
    return TorrentEnv {_completed = c, _completedSig = cSig, _downloading = d}

newDownloading :: Int -> Int -> IO (TVar (IM.IntMap (TVar PieceBuffer)))
newDownloading s p = do
    mapM f [0..s-1] >>= newTVarIO . IM.fromList
    where f i = (i,) <$> newTVarIO (emptyBuffer p)
