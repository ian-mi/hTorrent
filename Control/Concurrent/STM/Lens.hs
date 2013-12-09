module Control.Concurrent.STM.Lens where

import Morphisms

import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Monad.STM.Class
import Prelude

viewTVar :: (MonadReader e m, MonadSTM m) => Getting (TVar a) e (TVar a) -> m a
viewTVar v = view v >>= liftSTM . readTVar

viewTVarIO :: (MonadReader e m, MonadIO m) => Getting (TVar a) e (TVar a) -> m a
viewTVarIO v = view v >>= liftIO . readTVarIO

infixr 8 &.=
(&.=) :: (MonadReader e m, MonadSTM m) =>
    Getting (TVar a) e (TVar a) -> a -> m ()
v &.= a = view v >>= liftSTM . flip writeTVar a

infixr 8 !.=
(!.=) :: (MonadReader e m, MonadIO m) =>
    Getting (TVar a) e (TVar a) -> a -> m ()
v !.= a = embedReader liftIO (v &.= a)

infixr 8 &%=
(&%=) :: (MonadReader e m, MonadSTM m) =>
    Getting (TVar a) e (TVar a) -> (a -> a) -> m ()
v &%= f = view v >>= liftSTM . flip modifyTVar f

infixr 8 !%=
(!%=) :: (MonadReader e m, MonadIO m) =>
    Getting (TVar a) e (TVar a) -> (a -> a) -> m ()
v !%= f = embedReader liftIO (v &%= f)
