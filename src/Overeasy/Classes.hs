-- | Assorted abstract nonsense
module Overeasy.Classes
  ( BoundedJoinSemilattice
  , ApplyAction (..)
  ) where

-- A "bounded join semilattice":
-- mempty is bottom element of lattice, and mappend/<> is lattice join \/
-- No methods but should obey:
--   forall d. mempty <= d
--   forall d1, d2. d1 <= (d1 \/ d2), d2 <= (d1 /\ d2)
--   join is idempotent, associative, commutative, has mempty as unit,
--     and returns least upper bound
class (Ord d, Monoid d) => BoundedJoinSemilattice d

-- The trivial lattice
instance BoundedJoinSemilattice ()

-- A monoidal action to update state
-- Should obey:
--   applyAction mempty = id
--   applyAction (p1 <> p2) = applyAction p2 . applyAction p1
class Monoid p => ApplyAction p s where
  applyAction :: p -> s -> s

-- The trivial action
instance ApplyAction () s where
  applyAction = const id
