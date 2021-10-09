module Overeasy.StateUtil
  ( stateFail
  , stateFailBool
  , stateLens
  ) where

import Control.Monad.State.Strict (MonadState (..), State, runState)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)

stateFail :: (s -> Maybe (b, s)) -> State s (Maybe b)
stateFail f = do
  s <- get
  case f s of
    Nothing -> pure Nothing
    Just (b, s') -> put s' >> pure (Just b)

stateFailBool :: (s -> Maybe s) -> State s Bool
stateFailBool f = do
  s <- get
  case f s of
    Nothing -> pure False
    Just s' -> put s' >> pure True

stateLens :: Lens' s a -> State a b -> State s b
stateLens l act = state $ \s ->
  let (b, a') = runState act (view l s)
      s' = set l a' s
  in (b, s')
