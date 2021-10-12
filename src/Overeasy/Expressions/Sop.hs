{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Expressions.Sop
  ( Prod (..)
  , matchProdLeaf
  , matchProdRec
  , SopF (..)
  , Sop (..)
  , SopLike (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)

data Prod a r =
    ProdLeaf !a
  | ProdRec r
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Bifunctor Prod where
  bimap f g = \case
    ProdLeaf a -> ProdLeaf (f a)
    ProdRec r -> ProdRec (g r)

instance Bifoldable Prod where
  bifoldr f g z = \case
    ProdLeaf a -> f a z
    ProdRec r -> g r z

instance Bitraversable Prod where
  bitraverse f g = \case
    ProdLeaf a -> fmap ProdLeaf (f a)
    ProdRec r -> fmap ProdRec (g r)

matchProdLeaf :: Prod a r -> Maybe a
matchProdLeaf = \case
  ProdLeaf a -> Just a
  ProdRec _ -> Nothing

matchProdRec :: Prod a r -> Maybe r
matchProdRec = \case
  ProdLeaf _ -> Nothing
  ProdRec r -> Just r

data SopF b a r = SopF
  { sopLabel :: !b
  , sopArgs :: !(Seq (Prod a r))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

instance Bifunctor (SopF b) where
  bimap f g (SopF b gs) = SopF b (fmap (bimap f g) gs)

instance Bifoldable (SopF b) where
  bifoldr f g z (SopF _ gs) = foldr (flip (bifoldr f g)) z gs

instance Bitraversable (SopF b) where
  bitraverse f g (SopF b gs) = fmap (SopF b) (traverse (bitraverse f g) gs)

newtype Sop b a = Sop { unSop :: SopF b a (Sop b a) }
  deriving newtype (Eq, Show, NFData)

instance Functor (Sop b) where
  fmap f = go where
    go = Sop . bimap f go . unSop

instance Foldable (Sop b) where
  foldr f z0 x0 = go x0 z0 where
    go x z = bifoldr f go z (unSop x)

instance Traversable (Sop b) where
  traverse f = go where
    go = fmap Sop . bitraverse f go . unSop

type instance Base (Sop b a) = SopF b a

instance Recursive (Sop b a) where
  project = unSop

instance Corecursive (Sop b a) where
  embed = Sop

class (Recursive t, Corecursive t, Monad n, Traversable (Base t)) => SopLike b a n t | t -> b a n where
  toSopF :: Base t t -> SopF b a t
  fromSopF :: SopF b a t -> n (Base t t)
  toSop :: t -> Sop b a
  toSop = Sop . fmap toSop . toSopF . project
  fromSop :: Sop b a -> n t
  fromSop = fmap embed . (traverse fromSop >=> fromSopF) . unSop
