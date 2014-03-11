{-# LANGUAGE UndecidableInstances #-}

module HTorrent.Exception where

import ClassyPrelude
import qualified Control.Monad.Exception.Synchronous as E
import Control.Monad.Morph
import Control.Monad.Reader

data Except e a = Success a | Exception e
type ExceptT = E.ExceptionalT

fromExceptional :: E.Exceptional e a -> Except e a
fromExceptional (E.Success a) = Success a
fromExceptional (E.Exception e) = Exception e

instance MFunctor (E.ExceptionalT e) where
    hoist f = E.ExceptionalT . f . E.runExceptionalT

class Monad m => MonadExcept e m | m -> e where
    throw :: e -> m ()

instance Monad m => MonadExcept e (E.ExceptionalT e m) where
    throw = E.throwT

instance MonadExcept e m => MonadExcept e (ReaderT r m) where
    throw = lift . throw

instance MonadReader r m => MonadReader r (E.ExceptionalT e m) where
    ask = lift ask
    local f = E.ExceptionalT . local f . E.runExceptionalT
    reader = lift . reader

instance MonadIO m => MonadIO (E.ExceptionalT e m) where
    liftIO = lift . liftIO

instance (Show e, Show a) => Show (Except e a) where
    show (Success a) = "Success: " ++ show a
    show (Exception e) = "Exception: " ++ show e

assert :: MonadExcept e m => e -> Bool -> m ()
assert e p = unless p (throw e)

mapExceptT :: Monad m => (e0 -> e1) -> ExceptT e0 m a -> ExceptT e1 m a
mapExceptT = E.mapExceptionT

except :: Exception e => IO a -> ExceptT e IO a
except m = E.ExceptionalT (catch (E.Success <$> m) (return . E.Exception))

bracket :: Monad m =>
    ExceptT e m h -> (h -> ExceptT e m ()) -> (h -> ExceptT e m a) -> ExceptT e m a
bracket = E.bracketT

try :: Monad m => ExceptT e m a -> m (Except e a)
try = liftM fromExceptional . E.tryT
