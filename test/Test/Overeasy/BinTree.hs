{-# LANGUAGE DeriveAnyClass #-}

module Test.Overeasy.BinTree
  ( BinTreeF (..)
  , BinTree (..)
  , pattern BinTreeLeaf
  , pattern BinTreeBranch
  )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data BinTreeF a r
  = BinTreeLeafF !a
  | BinTreeBranchF r r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

instance Bifunctor BinTreeF where
  bimap f g = \case
    BinTreeLeafF a -> BinTreeLeafF (f a)
    BinTreeBranchF x y -> BinTreeBranchF (g x) (g y)

instance Bifoldable BinTreeF where
  bifoldr f g z = \case
    BinTreeLeafF a -> f a z
    BinTreeBranchF x y -> g x (g y z)

instance Bitraversable BinTreeF where
  bitraverse f g = \case
    BinTreeLeafF a -> fmap BinTreeLeafF (f a)
    BinTreeBranchF x y -> liftA2 BinTreeBranchF (g x) (g y)

newtype BinTree a = BinTree {unBinTree :: BinTreeF a (BinTree a)}
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

pattern BinTreeLeaf :: a -> BinTree a
pattern BinTreeLeaf a = BinTree (BinTreeLeafF a)

pattern BinTreeBranch :: BinTree a -> BinTree a -> BinTree a
pattern BinTreeBranch a b = BinTree (BinTreeBranchF a b)

{-# COMPLETE BinTreeLeaf, BinTreeBranch #-}

instance Functor BinTree where
  fmap f = go
   where
    go = BinTree . bimap f go . unBinTree

instance Foldable BinTree where
  foldr f z0 x0 = go x0 z0
   where
    go x z = bifoldr f go z (unBinTree x)

instance Traversable BinTree where
  traverse f = go
   where
    go = fmap BinTree . bitraverse f go . unBinTree

type instance Base (BinTree a) = BinTreeF a

instance Recursive (BinTree a) where
  project = unBinTree

instance Corecursive (BinTree a) where
  embed = BinTree
