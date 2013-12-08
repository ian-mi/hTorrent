module Peer.RequestBuffer (BufEnv(BufEnv), bufferRequests) where

import Morphisms

import Control.Concurrent.STM
import Control.Concurrent.STM.Lens
import Control.Applicative
import Control.Lens
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM.Class
import Data.List
import Data.Maybe

data BufEnv a = BufEnv {    _requested :: TQueue a,
                            _cancelled :: TQueue a,
                            _nextRequest :: TMVar a }

newtype BufState a = BufState { _bufferedRequests :: [a]}

$(makeLenses ''BufEnv)
$(makeLenses ''BufState)

bufferRequests :: (Eq a, MonadReader (BufEnv a) m, MonadIO m) => m ()
bufferRequests = evalStateT (forever buffer) (BufState [])

buffer :: ( Eq a,
            MonadReader (BufEnv a) m,
            MonadState (BufState a) m,
            MonadIO m) => m ()
buffer = do
    view nextRequest >>= liftIO . waitEmptyTMVar
    liftM2 (flip putTMVar) next (view nextRequest) >>= liftIO . atomically

next :: (   Eq a,
            MonadReader (BufEnv a) m,
            MonadState (BufState a) m,
            MonadIO m) => m a
next = do
    embedReader (embedState liftIO) getRequests
    use bufferedRequests >>= maybe next return . listToMaybe

getRequests :: (Eq a,
                MonadReader (BufEnv a) m,
                MonadState (BufState a) m,
                MonadSTM m) => m ()
getRequests = do
    rs <- liftM2 (++) (use bufferedRequests) (view requested >>= takeTQueue)
    cs <- view cancelled >>= takeTQueue
    if null rs && null cs then liftSTM retry else bufferedRequests .= rs \\ cs

waitEmptyTMVar :: MonadSTM m => TMVar a -> m ()
waitEmptyTMVar v = liftSTM (isEmptyTMVar v >>= guard . not)

takeTQueue :: MonadSTM m => TQueue a -> m [a]
takeTQueue = liftSTM . unfoldM . tryReadTQueue
