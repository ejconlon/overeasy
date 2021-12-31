{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We redefine Free here because we prefer undeciable instances
-- to having to derive 'Eq1' and so on.
-- See https://hackage.haskell.org/package/free-5.1.7/docs/Control-Monad-Trans-Free.html
module Overeasy.Expressions.Free
  ( FreeF (..)
  , Free (..)
  , pattern FreeEmbed
  , pattern FreePure
  , substFree
  , liftFree
  , iterFree
  , iterFreeM
  , FreeT (..)
  , liftFreeT
  , iterFreeT
  , hoistFreeT
  , transFreeT
  , joinFreeT
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | The recursive layer of a free functor
data FreeF f a r =
    FreePureF !a
  | FreeEmbedF !(f r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData, Hashable)

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
deriving stock instance (Show (f (Free f a)), Show a) => Show (Free f a)
deriving newtype instance (NFData (f (Free f a)), NFData a) => NFData (Free f a)
deriving newtype instance (Hashable (f (Free f a)), Hashable a) => Hashable (Free f a)

instance Functor f => Functor (Free f) where
  fmap f = go where
    go = Free . bimap f go . unFree

instance Functor f => Applicative (Free f) where
  pure = Free . FreePureF
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = pure
  Free m >>= f = case m of
    FreePureF a -> f a
    FreeEmbedF g -> Free (FreeEmbedF (fmap (>>= f) g))

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

-- | Fills all the holes in the free functor
substFree :: (Corecursive t, f ~ Base t) => (a -> t) -> Free f a -> t
substFree s = go where
  go = \case
    FreePure a -> s a
    FreeEmbed fr -> embed (fmap go fr)

-- | A version of lift that can be used with just a Functor for f
liftFree :: Functor f => f a -> Free f a
liftFree = FreeEmbed . fmap FreePure

-- | Tear down a free monad using iteration
iterFree :: Functor f => (f a -> a) -> Free f a -> a
iterFree f = go where
  go (Free x) =
    case x of
      FreePureF a -> a
      FreeEmbedF z -> f (fmap go z)

-- | Like iterFree for monadic values
iterFreeM :: (Functor f, Monad m) => (f (m a) -> m a) -> Free f a -> m a
iterFreeM f = go where
  go (Free x) =
    case x of
      FreePureF a -> pure a
      FreeEmbedF z -> f (fmap go z)

newtype FreeT f m a = FreeT { unFreeT :: m (FreeF f a (FreeT f m a)) }

deriving newtype instance Eq (m (FreeF f a (FreeT f m a))) => Eq (FreeT f m a)
deriving stock instance Show (m (FreeF f a (FreeT f m a))) => Show (FreeT f m a)
deriving newtype instance NFData (m (FreeF f a (FreeT f m a))) => NFData (FreeT f m a)
deriving newtype instance Hashable (m (FreeF f a (FreeT f m a))) => Hashable (FreeT f m a)

instance (Functor f, Functor m) => Functor (FreeT f m) where
  fmap f = go where
    go = FreeT . fmap (bimap f go) . unFreeT

instance (Functor f, Monad m) => Applicative (FreeT f m) where
  pure = FreeT . pure . FreePureF
  (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
  return = pure
  FreeT mm >>= f = FreeT $ mm >>= \case
    FreePureF a -> unFreeT (f a)
    FreeEmbedF z -> pure (FreeEmbedF (fmap (>>= f) z))

instance (Foldable f, Foldable m) => Foldable (FreeT f m) where
  foldr f z0 x0 = go x0 z0 where
    go x z = foldr (flip (bifoldr f go)) z (unFreeT x)

instance (Traversable f, Traversable m) => Traversable (FreeT f m) where
  traverse f = go where
    go = fmap FreeT . traverse (bitraverse f go) . unFreeT

liftFreeT :: (Functor f, Applicative m) => f a -> FreeT f m a
liftFreeT = FreeT . pure . FreeEmbedF . fmap (FreeT . pure . FreePureF)

iterFreeT :: (Functor f, Monad m) => (f (m a) -> m a) -> FreeT f m a -> m a
iterFreeT f = go where
  go (FreeT m) = m >>= \case
     FreePureF a -> pure a
     FreeEmbedF z -> f (fmap go z)

hoistFreeT :: (Functor f, Functor m) => (forall a. m a -> n a) -> FreeT f m b -> FreeT f n b
hoistFreeT g = go where
  go (FreeT m) = FreeT $ g $ flip fmap m $ \case
     FreePureF a -> FreePureF a
     FreeEmbedF z -> FreeEmbedF (fmap go z)

transFreeT :: (Functor g, Monad m) => (forall a. f a -> g a) -> FreeT f m b -> FreeT g m b
transFreeT g = go where
  go (FreeT m) = FreeT $ flip fmap m $ \case
     FreePureF a -> FreePureF a
     FreeEmbedF z -> FreeEmbedF (fmap go (g z))

joinFreeT :: (Monad m, Traversable f) => FreeT f m a -> m (Free f a)
joinFreeT x = unFreeT x >>= \case
   FreePureF a -> pure (FreePure a)
   FreeEmbedF z -> fmap FreeEmbed (traverse joinFreeT z)
