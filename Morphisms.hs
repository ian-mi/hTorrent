module Morphisms where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

embedReader :: MonadReader r m => (forall a. n a -> m a) -> ReaderT r n a -> m a
embedReader f r = ask >>= f . runReaderT r

embedState :: MonadState s m => (forall a. n a -> m a) -> StateT s n a -> m a
embedState f s = get >>= f . runStateT s >>= state . const

zoomState :: MonadState s m => Lens' s a -> State a b -> m b
zoomState l = (l %%=) . runState
