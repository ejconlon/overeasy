-- | Some useful functions for state
module Overeasy.StateUtil
  ( stateFail
  , stateFailChanged
  , stateLens
  , RSM
  , runRSM
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import Control.Monad.State.Strict (MonadState (..), State, runState)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)
import Overeasy.Classes (Changed (..))

-- | Embeds a function that may fail in a stateful context
stateFail :: MonadState s m => (s -> Maybe (b, s)) -> m (Maybe b)
stateFail f = do
  s <- get
  case f s of
    Nothing -> pure Nothing
    Just (b, s') -> put s' >> pure (Just b)

-- | Embeds a function that may fail in a stateful context with change tracking
stateFailChanged :: MonadState s m => (s -> Maybe s) -> m Changed
stateFailChanged f = do
  s <- get
  case f s of
    Nothing -> pure ChangedNo
    Just s' -> put s' >> pure ChangedYes

-- | Embeds a stateful action in a larger context
stateLens :: MonadState s m => Lens' s a -> State a b -> m b
stateLens l act = state $ \s ->
  let (b, a') = runState act (view l s)
      s' = set l a' s
  in (b, s')

-- | A Reader-State monad
newtype RSM r s a = RSM { unRSM :: ReaderT r (State s) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState s)

-- | Runs the Reader-State monad
runRSM :: RSM r s a -> r -> s -> (a, s)
runRSM (RSM m) = runState . runReaderT m
