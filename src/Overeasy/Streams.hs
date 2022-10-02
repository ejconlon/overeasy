module Overeasy.Streams
  ( chooseWith
  , choose
  , Stream
  , streamAll
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Logic (LogicT, MonadLogic, observeAllT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), State, runState)

newtype M r s a = M { unM :: ReaderT r (State s) a }
  deriving newtype (
    Functor, Applicative, Monad,
    MonadReader r, MonadState s)

runM :: M r s a -> r -> s -> (a, s)
runM m r = runState (runReaderT (unM m) r)

chooseWith :: (Foldable f, Alternative m) => f a -> (a -> m b) -> m b
chooseWith fa f = foldr ((<|>) . f) empty fa

choose :: (Foldable f, Alternative m) => f a -> m a
choose fa = chooseWith fa pure

newtype Stream r s a = Stream { unStream :: LogicT (M r s) a }
  deriving newtype (
    Functor, Applicative, Monad,
    MonadReader r, MonadState s, MonadLogic,
    Alternative, MonadPlus, Semigroup, Monoid)

streamAll :: Stream r s a -> r -> s -> [a]
streamAll stream env st = fst (runM (observeAllT (unStream stream)) env st)
