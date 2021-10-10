-- | Some useful functions for recursion
module Overeasy.Recursion
  ( Whole
  , RecursiveWhole
  , foldWhole
  , foldWholeM
  , foldWholeTrackM
  ) where

import Data.Functor.Foldable (Base, Recursive (..), fold)

-- | Often 'f' is primary, not 't'. Relate them with this constraint.
type Whole t f = (f ~ Base t)

-- | Constraint for recursive structures
type RecursiveWhole t f = (Recursive t, Whole t f)

-- | It's just 'fold' with our constraints.
foldWhole :: RecursiveWhole t f => (f a -> a) -> t -> a
foldWhole = fold

-- | Traverses a recursive structure
foldWholeM :: (RecursiveWhole t f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
foldWholeM h = go where
  go t = do
    let ft = project t
    fa <- traverse go ft
    h fa

-- | Traverses and sequences a recursive structure. Useful to track changes bottom-up.
foldWholeTrackM :: (RecursiveWhole t f, Traversable f, Monad m, Monoid w) => (f a -> m (w, a)) -> t -> m (w, a)
foldWholeTrackM h = go where
  go t = do
    let ft = project t
    fca <- traverse go ft
    let (c1, fa) = sequenceA fca
    (c2, a) <- h fa
    pure (c1 <> c2, a)
