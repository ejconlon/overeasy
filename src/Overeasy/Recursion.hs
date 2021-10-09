module Overeasy.Recursion
  ( Whole
  , RecursiveWhole
  , foldWhole
  , foldWholeM
  , foldWholeTrackM
  , foldStateChangeM
  ) where

import Control.Monad.State.Strict (State)
import Data.Functor.Foldable (Base, Recursive (..), fold)
import Overeasy.StateUtil (Changed)

type Whole t f = (f ~ Base t)

type RecursiveWhole t f = (Recursive t, Whole t f)

foldWhole :: RecursiveWhole t f => (f a -> a) -> t -> a
foldWhole = fold

foldWholeM :: (RecursiveWhole t f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
foldWholeM h = go where
  go t = do
    let ft = project t
    fa <- traverse go ft
    h fa

foldWholeTrackM :: (RecursiveWhole t f, Traversable f, Monad m, Monoid w) => (f a -> m (w, a)) -> t -> m (w, a)
foldWholeTrackM h = go where
  go t = do
    let ft = project t
    fca <- traverse go ft
    let (c1, fa) = sequenceA fca
    (c2, a) <- h fa
    pure (c1 <> c2, a)

foldStateChangeM :: (RecursiveWhole t f, Traversable f) => (f a -> State s (Changed, a)) -> t -> State s (Changed, a)
foldStateChangeM = foldWholeTrackM
