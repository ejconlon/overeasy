module Overeasy.Recursion
  ( Whole
  , RecursiveWhole
  , foldWhole
  , foldWholeM
  ) where

import Data.Functor.Foldable (Base, Recursive (..), fold)

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
