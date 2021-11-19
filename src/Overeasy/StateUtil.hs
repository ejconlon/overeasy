-- | Some useful functions for state
module Overeasy.StateUtil
  ( stateFail
  , stateOption
  , stateFailChanged
  -- , stateLens
  , stateFold
  ) where

import Control.Monad (foldM)
import Control.Monad.State.Strict (State, get, put, runState, state)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)
import Overeasy.Classes (Changed (..))

-- | Embeds a function that may fail in a stateful context
stateFail :: (s -> Maybe (b, s)) -> State s (Maybe b)
stateFail f = do
  s <- get
  case f s of
    Nothing -> pure Nothing
    Just (b, s') -> put s' >> pure (Just b)

-- | Embeds a function that may fail in a stateful context
stateOption :: (s -> (b, Maybe s)) -> State s b
stateOption f = do
  s <- get
  let (b, ms) = f s
  case ms of
    Nothing -> pure ()
    Just s' -> put s'
  pure b

-- | Embeds a function that may fail in a stateful context with change tracking
stateFailChanged :: (s -> Maybe s) -> State s Changed
stateFailChanged f = do
  s <- get
  case f s of
    Nothing -> pure ChangedNo
    Just s' -> put s' >> pure ChangedYes

-- -- | Embeds a stateful action in a larger context
-- stateLens :: Lens' s a -> State a b -> State s b
-- stateLens l act = state $ \s ->
--   let (b, a') = runState act (view l s)
--       s' = set l a' s
--   in (b, s')

-- | 'foldM' specialized and flipped.
stateFold :: Foldable t => b -> t a -> (b -> a -> State s b) -> State s b
stateFold b as f = foldM f b as
{-# INLINE stateFold #-}
