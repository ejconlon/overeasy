{-# LANGUAGE DeriveAnyClass #-}

-- | A grab bag of fun stuff.
module Overeasy.Util
  ( Whole
  , RecursiveWhole
  , foldWholeM
  , Changed (..)
  , stateFail
  , stateOption
  , stateFailChanged
  , stateFold
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, forM_)
import Control.Monad.State.Strict (State, get, put)
import Data.Functor.Foldable (Base, Recursive (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Often 'f' is primary, not 't'. Relate them with this constraint.
type Whole t f = (f ~ Base t)

-- | Constraint for recursive structures
type RecursiveWhole t f = (Recursive t, Whole t f)

-- | Traverses a recursive structure
foldWholeM :: (RecursiveWhole t f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
foldWholeM h = go
 where
  go t = do
    let ft = project t
    fa <- traverse go ft
    h fa

-- | A nicely-named 'Bool' for tracking state changes
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
  forM_ ms put
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
stateFold :: (Foldable t) => b -> t a -> (b -> a -> State s b) -> State s b
stateFold b as f = foldM f b as
{-# INLINE stateFold #-}
