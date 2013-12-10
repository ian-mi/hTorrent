module Peer.Request (requestThread, RequestEnv(RequestEnv)) where

import Control.Concurrent.STM.Lens
import Morphisms
import Peer.Env
import Peer.Message
import Piece
import Torrent.Env

import HTorrentPrelude
import Control.Concurrent.STM.State
import Control.Monad.STM.Class
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S

data RequestEnv = RequestEnv {
    _requests :: TQueue ChunkInd,
    _peer :: PeerEnv,
    _torrent :: TorrentEnv }
$(makeLenses ''RequestEnv)

data RequestState = RequestState {
    _curPieceNum :: Int,
    _curPiece :: TVar PieceBuffer }
$(makeLenses ''RequestState)

numPipeline :: Int
numPipeline = 1

requestThread :: (MonadReader RequestEnv m, MonadIO m) => m ()
requestThread = evalStateT makeRequests Nothing

makeRequests ::
    (MonadReader RequestEnv m, MonadState (Maybe RequestState) m, MonadIO m)
    => m ()
makeRequests =
    forever (embedReader (embedState (liftIO . atomically)) makeRequest)

makeRequest ::
    (MonadReader RequestEnv m, MonadState (Maybe RequestState) m, MonadSTM m)
    =>  m ()
makeRequest = runMaybeT nextRequest >>= maybe nextPiece waitRequest

nextRequest :: (MonadState (Maybe RequestState) m, MonadSTM m)
    => MaybeT m ChunkInd
nextRequest = do
    RequestState p b <- MaybeT get
    (i, l) <- MaybeT (tvarState allocNext b)
    return (ChunkInd p i l)

waitRequest :: (MonadReader RequestEnv m, MonadSTM m) => ChunkInd -> m ()
waitRequest i = do
    viewTVar (peer . remoteState . choked) >>= liftSTM . guard . not
    viewTVar (peer . pendingRequests) >>= liftSTM . guard . (< numPipeline) . S.size
    peer . pendingRequests &%= S.insert i
    view requests >>= liftSTM . flip writeTQueue i

nextPiece ::
    (MonadReader RequestEnv m, MonadState (Maybe RequestState) m, MonadSTM m)
    => m ()
nextPiece = do
    ps <- interestedPieces 
    i <- viewTVar (peer . localState . interested)
    case listToMaybe ps of
        Just (p, b) -> do
            unless i (peer . localState . interested &.= True)
            put (Just (RequestState p b))
        Nothing -> do
            liftM (i ||) (gets isJust) >>= liftSTM . guard
            peer . localState . interested &.= False
            put Nothing

interestedPieces :: (MonadReader RequestEnv m, MonadSTM m) =>
    m [(Int, TVar PieceBuffer)]
interestedPieces = do
    d <- viewTVar (torrent . downloading)
    i <- liftM (flip filterKeys d . flip IS.member) (viewTVar (peer . pieces))
    liftSTM (filterM (liftM (not . full) . readTVar . snd) (IM.toList i))

filterKeys :: (Int -> Bool) -> IntMap a -> IntMap a
filterKeys = IM.filterWithKey . (const .)
