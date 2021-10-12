{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We redefine Free here because we prefer undeciable instances
-- to having to derive 'Eq1' and so on.
module Overeasy.Expressions.Free
  ( FreeF (..)
  , Free (..)
  , pattern FreeEmbed
  , pattern FreePure
  , freeVars
  , freeSubst
  ) where

import Control.DeepSeq (NFData)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | The recursive layer of a free functor
data FreeF f a r =
    FreePureF !a
  | FreeEmbedF !(f r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Functor f => Bifunctor (FreeF f) where
  bimap f g = \case
    FreePureF a -> FreePureF (f a)
    FreeEmbedF fr -> FreeEmbedF (fmap g fr)

instance Foldable f => Bifoldable (FreeF f) where
  bifoldr f g z = \case
    FreePureF a -> f a z
    FreeEmbedF fr -> foldr g z fr

instance Traversable f => Bitraversable (FreeF f) where
  bitraverse f g = \case
    FreePureF a -> fmap FreePureF (f a)
    FreeEmbedF fr -> fmap FreeEmbedF (traverse g fr)

-- | The free functor. Use patterns 'FreePure' and 'FreeEmbed' to match and construct.
newtype Free f a = Free { unFree :: FreeF f a (Free f a) }

pattern FreePure :: a -> Free f a
pattern FreePure a = Free (FreePureF a)

pattern FreeEmbed :: f (Free f a) -> Free f a
pattern FreeEmbed fr = Free (FreeEmbedF fr)

{-# COMPLETE FreePure, FreeEmbed #-}

deriving newtype instance (Eq (f (Free f a)), Eq a) => Eq (Free f a)
deriving newtype instance (Show (f (Free f a)), Show a) => Show (Free f a)
deriving newtype instance (NFData (f (Free f a)), NFData a) => NFData (Free f a)

instance Functor f => Functor (Free f) where
  fmap f = go where
    go = Free . bimap f go . unFree

instance Foldable f => Foldable (Free f) where
  foldr f z0 x0 = go x0 z0 where
    go x z = bifoldr f go z (unFree x)

instance Traversable f => Traversable (Free f) where
  traverse f = go where
    go = fmap Free . bitraverse f go . unFree

type instance Base (Free f a) = (FreeF f a)

instance Functor f => Recursive (Free f a) where
  project = unFree

instance Functor f => Corecursive (Free f a) where
  embed = Free

-- | The set of all holes in this free functor
freeVars :: (Foldable f, Eq a, Hashable a) => Free f a -> HashSet a
freeVars = HashSet.fromList . toList

-- | Fills all the holes in the free functor
freeSubst :: (Corecursive t, f ~ Base t) => (a -> t) -> Free f a -> t
freeSubst s = go where
  go = \case
    FreePure a -> s a
    FreeEmbed fr -> embed (fmap go fr)
