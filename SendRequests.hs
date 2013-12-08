module SendRequests(runRequests, RequestEnv) where

import Morphisms
import PeerMessage
import Piece

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM.Class
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe

data RequestEnv = RequestEnv {
    _pending :: TVar (S.Set ChunkInd),
    _requests :: TQueue ChunkInd,
    _peerPieces :: TVar IS.IntSet,
    _downloading :: TVar (IM.IntMap (TVar PieceBuffer)),
    _interested :: TVar Bool }
$(makeLenses ''RequestEnv)

newtype RequestState = RequestState { _cur :: Maybe (Int, TVar PieceBuffer) }
$(makeLenses ''RequestState)

runRequests :: (MonadReader RequestEnv m, MonadIO m) => m ()
runRequests = evalStateT (forever runRequest) (RequestState Nothing)

runRequest :: (MonadState RequestState m, MonadReader RequestEnv m, MonadIO m)
    => m ()
runRequest = embedState (embedReader (liftIO . atomically)) $ do
    ps <- pieces
    updateInterested (null ps) <|> request ps

pieces :: (MonadReader RequestEnv m, MonadSTM m) => m [(Int, TVar PieceBuffer)]
pieces = do
    p <- view peerPieces >>= liftSTM . readTVar
    d <- view downloading >>= liftSTM . readTVar
    let f (i, b) = liftSTM ((IS.member i p &&) . not . full <$> readTVar b)
    filterM f (IM.toList d)

updateInterested :: (MonadReader RequestEnv m, MonadSTM m) => Bool -> m ()
updateInterested b = do
    i <- view interested >>= liftSTM . readTVar
    p <- view pending >>= liftSTM . readTVar
    let i' = not (b && S.null p)
    if i == i'
        then liftSTM retry
        else view interested >>= liftSTM . flip writeTVar i'

request :: (MonadReader RequestEnv m, MonadState RequestState m, MonadSTM m)
    => [(Int, TVar PieceBuffer)] -> m ()
request ps = do
    c <- use cur
    case c of
        Nothing -> maybe (liftSTM retry) (cur ?=) (listToMaybe ps)
        Just (i, b) -> makeRequest b >>= maybe (cur .= Nothing) (writeRequest i)

writeRequest :: (MonadReader RequestEnv m, MonadSTM m) =>
    Int -> (Int, Int) -> m ()
writeRequest p (i, l) = do
    view requests >>= liftSTM . flip writeTQueue c
    view pending >>= liftSTM . flip modifyTVar' (S.insert c)
    where c = ChunkInd (fromIntegral p) (fromIntegral i) (fromIntegral l)

makeRequest :: MonadSTM m => TVar PieceBuffer -> m (Maybe (Int, Int))
makeRequest b = liftSTM $ do
    (r, s) <- runState (nextRequest (2^14)) <$> readTVar b 
    r <$ writeTVar b s
