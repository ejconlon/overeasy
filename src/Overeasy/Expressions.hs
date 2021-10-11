{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various expression types and functions.
-- We redefine free stuff here because we prefer undeciable instances
-- to having to derive 'Eq1' and so on.
module Overeasy.Expressions
  ( FreeF (..)
  , Free (..)
  , pattern FreeEmbed
  , pattern FreePure
  , freeVars
  , freeSubst
  , Atom (..)
  , SexpF (..)
  , Sexp (..)
  , pattern SexpAtom
  , pattern SexpList
  , sexpAbstract
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
import Data.Sequence (Seq)
import Data.Text (Text)
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

type instance Base (Free f a) = (FreeF f a)

instance Functor f => Recursive (Free f a) where
  project = unFree

instance Functor f => Corecursive (Free f a) where
  embed = Free

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

-- | The set of all holes in this free functor
freeVars :: (Foldable f, Eq a, Hashable a) => Free f a -> HashSet a
freeVars = HashSet.fromList . toList

-- | Fills all the holes in the free functor
freeSubst :: (Corecursive t, f ~ Base t) => (a -> t) -> Free f a -> t
freeSubst s = go where
  go = \case
    FreePure a -> s a
    FreeEmbed fr -> embed (fmap go fr)

-- | A sexp atom
data Atom =
    AtomInteger !Integer
  | AtomDouble !Double
  | AtomString !Text
  | AtomIdent !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | The sexp functor
data SexpF r =
    SexpAtomF !Atom
  | SexpListF !(Seq r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- | A sexp. Use patterns 'SexpAtom' and 'SexpList' to match and construct.
newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving newtype (Eq, Show, NFData)

pattern SexpAtom :: Atom -> Sexp
pattern SexpAtom a = Sexp (SexpAtomF a)

pattern SexpList :: Seq Sexp -> Sexp
pattern SexpList ss = Sexp (SexpListF ss)

{-# COMPLETE SexpAtom, SexpList #-}

type instance Base Sexp = SexpF

instance Recursive Sexp where
  project = unSexp

instance Corecursive Sexp where
  embed = Sexp

-- | Add holes for select identifiers
sexpAbstract :: (Text -> Maybe a) -> Sexp -> Free SexpF a
sexpAbstract f = go where
  go = \case
    SexpAtom a ->
      case a of
        AtomIdent i ->
          case f i of
            Nothing -> FreeEmbed (SexpAtomF a)
            Just v -> FreePure v
        _ -> FreeEmbed (SexpAtomF a)
    SexpList ss -> FreeEmbed (SexpListF (fmap go ss))
