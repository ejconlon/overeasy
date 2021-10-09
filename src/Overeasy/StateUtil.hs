module Overeasy.StateUtil
  ( Changed (..)
  , sequenceChanged
  , stateFail
  , stateFailChanged
  , stateLens
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState (..), State, runState)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)

data Changed = ChangedNo | ChangedYes
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance Semigroup Changed where
  c1 <> c2 =
    case c1 of
      ChangedYes -> ChangedYes
      _ -> c2

instance Monoid Changed where
  mempty = ChangedNo
  mappend = (<>)

sequenceChanged :: Traversable f => f (Changed, a) -> (Changed, f a)
sequenceChanged = sequenceA

stateFail :: (s -> Maybe (b, s)) -> State s (Maybe b)
stateFail f = do
  s <- get
  case f s of
    Nothing -> pure Nothing
    Just (b, s') -> put s' >> pure (Just b)

stateFailChanged :: (s -> Maybe s) -> State s Changed
stateFailChanged f = do
  s <- get
  case f s of
    Nothing -> pure ChangedNo
    Just s' -> put s' >> pure ChangedYes

stateLens :: Lens' s a -> State a b -> State s b
stateLens l act = state $ \s ->
  let (b, a') = runState act (view l s)
      s' = set l a' s
  in (b, s')
