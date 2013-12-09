module Control.Concurrent.STM.State where

import HTorrentPrelude
import Control.Monad.STM.Class

tvarState :: MonadSTM m => State s a -> TVar s -> m a
tvarState m v = liftSTM $ do
    (a, s) <- runState m <$> readTVar v
    a <$ writeTVar v s
