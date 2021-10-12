{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Expressions.Tree
  ( TreeF (..)
  , Tree
  , TreeLike (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)

data TreeF a r = TreeF !a !(Seq r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Bifunctor TreeF where
  bimap f g (TreeF a rs) = TreeF (f a) (fmap g rs)

instance Bifoldable TreeF where
  bifoldr f g z (TreeF a rs) = f a (foldr g z rs)

instance Bitraversable TreeF where
  bitraverse f g (TreeF a rs) = TreeF <$> f a <*> traverse g rs

newtype Tree a = Tree { unTree :: TreeF a (Tree a) }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Functor Tree where
  fmap f = go where
    go = Tree . bimap f go . unTree

instance Foldable Tree where
  foldr f z0 x0 = go x0 z0 where
    go x z = bifoldr f go z (unTree x)

instance Traversable Tree where
  traverse f = go where
    go = fmap Tree . bitraverse f go . unTree

type instance Base (Tree a) = TreeF a

instance Recursive (Tree a) where
  project = unTree

instance Corecursive (Tree a) where
  embed = Tree

class (Recursive t, Corecursive t, Monad n, Traversable (Base t)) => TreeLike a n t | t -> a n where
  toTreeF :: Base t t -> TreeF a t
  fromTreeF :: TreeF a t -> n (Base t t)
  toTree :: t -> Tree a
  toTree = Tree . fmap toTree . toTreeF . project
  fromTree :: Tree a -> n t
  fromTree = fmap embed . (traverse fromTree >=> fromTreeF) . unTree
