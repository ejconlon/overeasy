-- | Methods to match patterns in 'EGraph's
module Overeasy.Matching
  ( Pat
  -- , Subst
  -- , ESearcher (..)
  -- , EApplier (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState (..), State, gets)
import Data.Functor.Foldable (cata)
import Data.Hashable (Hashable)
import IntLike.MultiMap (IntLikeMultiMap)
import qualified IntLike.MultiMap as ILMM
import Overeasy.Assoc (Assoc, assocInsertInc, assocLookupByValue, assocNew)
import Overeasy.EGraph (EClassId)
import Overeasy.Source (Source, sourceAddInc, sourceNew)
import Unfree (Free, FreeF (..))

-- | A pattern is exactly the free monad over the expression functor
-- It has spots for var names ('FreePure') and spots for structural
-- pieces ('FreeEmbed')
type Pat f v = Free f v

-- | An opaque var id
-- Constructor exported for coercibility
newtype VarId = VarId { unVarId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

type PatPart f v = FreeF f v VarId

data MatchState f v = MatchState
  { msSrc :: !(Source VarId)
  , msAssoc :: !(Assoc VarId (PatPart f v))
  , msSol :: !(IntLikeMultiMap VarId EClassId)
  }

deriving stock instance (Eq v, Eq (f VarId)) => Eq (MatchState f v)
deriving stock instance (Show v, Show (f VarId)) => Show (MatchState f v)

matchStateNew :: MatchState f v
matchStateNew = MatchState (sourceNew (VarId 0)) assocNew ILMM.empty

type MatchM f v = State (MatchState f v)
type MatchC f v = (Traversable f, Eq v, Eq (f VarId), Hashable v, Hashable (f VarId))

matchEnsurePart :: MatchC f v => PatPart f v -> MatchM f v VarId
matchEnsurePart part = do
  mi <- gets (assocLookupByValue part . msAssoc)
  case mi of
    Just i -> pure i
    Nothing -> state $ \st ->
      let (i, src') = sourceAddInc (msSrc st)
          (_, assoc') = assocInsertInc i part (msAssoc st)
      in (i, st { msSrc = src', msAssoc = assoc' })

matchEnsurePat :: MatchC f v => Pat f v -> MatchM f v VarId
matchEnsurePat = cata go where
  go = \case
    FreePureF v -> matchEnsurePart (FreePureF v)
    FreeEmbedF fp -> do
      fi <- sequenceA fp
      matchEnsurePart (FreeEmbedF fi)

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
