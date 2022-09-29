-- | Methods to match patterns in 'EGraph's
module Overeasy.Matching
  ( Pat
  -- , Subst
  -- , ESearcher (..)
  -- , EApplier (..)
  ) where

import Unfree (Free)
-- import Data.HashSet (HashSet)
-- import Data.HashMap.Strict (HashMap)
-- import Overeasy.EGraph (EClassId, EGraph, egClasses)
-- import Control.Monad.State.Strict (evalState)
-- import Data.Sequence (Seq)

-- | A pattern is exactly the free monad over the expression functor
-- It has spots for var names ('FreePure') and spots for structural
-- pieces ('FreeEmbed')
type Pat f v = Free f v

-- patVars :: Foldable f => Pat f v -> HashSet v
-- patVars = error "TODO"

-- What ACTUALLY TO DO
-- given a Source of PatId, insert all the layers of the pattern into an assoc
-- let g be f with some spots fixed with v
-- This way you have a mapping PatId -> g PatId
-- for each class, for each of these partitions match against nodes, yielding a
-- sequence of (f (Either id v, EClassId)) for all that class
-- Now you have
--   Map EClassId (Seq (f (Either PatId v, EClassId)))
--   Map PatId (Set EClassId)
-- DFS this graph to produce Subst v (aka Map v EClassId) or just
-- data Anno = Anno !EClassId !(Pat f Anno)

-- -- | A substitution is a map of var names to class ids
-- type Subst v = HashMap v EClassId

-- applySubst :: Traversable f => Pat f v -> Either v (Free f EClassId)
-- applySubst = error "TODO"

-- class Monoid s => EMatcher d f v s where
--   matchOne :: Pat f v -> EGraph d f -> EClassId -> s
--   matchAll :: Pat f v -> EGraph d f -> s
--   matchAll pat eg = foldMap (matchOne pat eg) (evalState egClasses eg)

-- data SubstMatch d v = SubstMatch !EClassId !d !(Subst v)
--   deriving stock (Eq, Ord, Show)

-- newtype SubstMatches d v = SubstMatches { unSubstMatches :: Seq (SubstMatch d v) }
--   deriving stock (Show)
--   deriving newtype (Eq, Ord, Semigroup, Monoid)

-- instance EMatcher d f v (SubstMatches d v) where
--   matchOne pat eg cid = error "TODO"
