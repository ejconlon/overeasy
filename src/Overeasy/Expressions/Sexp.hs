{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Expressions.Sexp
  ( Atom (..)
  , SexpF (..)
  , Sexp (..)
  , pattern SexpAtom
  , pattern SexpList
  , sexpAbstract
  ) where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Hashable (Hashable)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Overeasy.Expressions.Sop (Prod (..), SopF (..), SopLike (..), matchProdRec)
import Overeasy.Expressions.Tree (TreeF (..), TreeLike (..))

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

data SexpElem =
    SexpElemList
  | SexpElemAtom !Atom
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance TreeLike SexpElem Maybe Sexp where
  toTreeF = \case
    SexpAtomF a -> TreeF (SexpElemAtom a) Empty
    SexpListF ss -> TreeF SexpElemList ss
  fromTreeF = \case
    TreeF (SexpElemAtom a) Empty -> Just (SexpAtomF a)
    TreeF SexpElemList ss -> Just (SexpListF ss)
    _ -> Nothing

data SexpLabel =
    SexpLabelAtom
  | SexpLabelList
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance SopLike SexpLabel Atom Maybe Sexp where
  toSopF = \case
    SexpAtomF a -> SopF SexpLabelAtom (Seq.singleton (ProdLeaf a))
    SexpListF ss -> SopF SexpLabelList (fmap ProdRec ss)
  fromSopF = \case
    SopF SexpLabelAtom (ProdLeaf a :<| Empty) -> Just (SexpAtomF a)
    SopF SexpLabelList rss -> fmap SexpListF (traverse matchProdRec rss)
    _ -> Nothing
