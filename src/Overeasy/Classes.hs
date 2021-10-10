{-# LANGUAGE DeriveAnyClass #-}

-- | Assorted abstract nonsense
module Overeasy.Classes
  ( Changed (..)
  , BoundedJoinSemilattice (..)
  , defaultJoinChanged
  , ApplyAction (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | A nicely-named 'Bool' for tracking state changes
data Changed = ChangedNo | ChangedYes
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance Semigroup Changed where
  c1 <> c2 =
    case c1 of
      ChangedYes -> ChangedYes
      _ -> c2

instance Monoid Changed where
  mempty = ChangedNo
  mappend = (<>)

-- A "bounded join semilattice":
-- mempty is bottom element of lattice, and mappend/<> is lattice join \/
-- Should obey:
--   forall d. mempty <= d
--   forall d1, d2. d1 <= (d1 \/ d2), d2 <= (d1 \/ d2)
--   joinChanged = defaultJoinChanged
--   join is idempotent, associative, commutative, has mempty as unit,
--     and returns least upper bound
class (Ord d, Monoid d) => BoundedJoinSemilattice d where
  -- | Change-aware join (default is fine)
  joinChanged :: d -> d -> (Changed, d)
  joinChanged = defaultJoinChanged

defaultJoinChanged :: (Eq d, Semigroup d) => d -> d -> (Changed, d)
defaultJoinChanged x y =
  let z = x <> y
      c = if x == z then ChangedNo else ChangedYes
  in (c, z)

-- The trivial lattice
instance BoundedJoinSemilattice () where
  joinChanged _ _ = (ChangedNo, ())

-- A monoidal action to update state
-- Should obey:
--   applyAction mempty = id
--   applyAction (p1 <> p2) = applyAction p2 . applyAction p1
class Monoid p => ApplyAction p s where
  applyAction :: p -> s -> s

-- The trivial action
instance ApplyAction () s where
  applyAction = const id
