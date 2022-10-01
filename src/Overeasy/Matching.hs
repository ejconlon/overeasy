{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Methods to match patterns in 'EGraph's (aka e-matching)
module Overeasy.Matching
  ( Pat
  , patVars
  , Subst
  , substVars
  , VarId (..)
  , MatchC
  , PatGraph (..)
  , patGraph
  , Match (..)
  , MatchPat (..)
  , MatchF (..)
  , MatchPatF (..)
  , matchVars
  , matchClasses
  , MatchSubst (..)
  , SolGraph (..)
  , solGraph
  , SolStream
  , nextSol
  , allSols
  , solve
  , match
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (MonadState (..), State, gets, runState)
import Data.Coerce (Coercible)
import Data.Foldable (fold, foldMap', foldl')
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Overeasy.Assoc (Assoc, assocEquiv, assocFootprint, assocFwd, assocInsertInc, assocLookupByValue, assocNew)
import Overeasy.EGraph (EClassId (..), EGraph, ENodeId (..), eciNodes, egClassMap, egNodeAssoc)
import Overeasy.EquivFind (efLookupRoot)
import Overeasy.Source (Source, sourceAddInc, sourceNew)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import Unfree (Free, FreeF (..))

-- | A pattern is exactly the free monad over the expression functor
-- It has spots for var names ('FreePure') and spots for structural
-- pieces ('FreeEmbed')
type Pat = Free

type PatF = FreeF

patVars :: (Foldable f, Eq v, Hashable v) => Pat f v -> HashSet v
patVars = foldMap' HashSet.singleton

type Subst a v = HashMap v a

substVars :: Subst a v -> HashSet v
substVars = HashMap.keysSet

-- | A match is a pattern annotated with classes
data Match a f v = Match
  { matchClass :: !a
  , matchPat :: !(MatchPat a f v)
  } deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq a, Eq v, Eq (f (Match a f v))) => Eq (Match a f v)
deriving stock instance (Show a, Show v, Show (f (Match a f v))) => Show (Match a f v)

data MatchPat a f v =
    MatchPatPure !v
  | MatchPatEmbed !(f (Match a f v))
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq v, Eq (f (Match a f v))) => Eq (MatchPat a f v)
deriving stock instance (Show v, Show (f (Match a f v))) => Show (MatchPat a f v)

-- | The base functor of 'Match'
data MatchF a f v r = MatchF
  { matchClassF :: !a
  , matchPatF :: !(MatchPatF f v r)
  } deriving stock (Functor, Foldable, Traversable)

data MatchPatF f v r =
    MatchPatPureF !v
  | MatchPatEmbedF !(f r)
  deriving stock (Functor, Foldable, Traversable)

type instance Base (Match a f v) = MatchF a f v

instance Functor f => Recursive (Match a f v) where
  project (Match cl mp) = MatchF cl $ case mp of
     MatchPatPure v -> MatchPatPureF v
     MatchPatEmbed f -> MatchPatEmbedF f

instance Functor f => Corecursive (Match a f v) where
  embed (MatchF cl mpf) = Match cl $ case mpf of
     MatchPatPureF v -> MatchPatPure v
     MatchPatEmbedF f -> MatchPatEmbed f

matchVars :: (Foldable f, Eq v, Hashable v) => Match a f v -> HashSet v
matchVars = foldMap' HashSet.singleton

matchClasses :: (Coercible a Int, Functor f, Foldable f) => Match a f v -> IntLikeSet a
matchClasses = cata go where
  go (MatchF cl mpf) = ILS.insert cl $ case mpf of
    MatchPatPureF _ -> ILS.empty
    MatchPatEmbedF fc -> fold fc

data MatchSubst a f v = MatchSubst
  { msMatch :: !(Match a f v)
  , msSubst :: !(Subst a v)
  }

deriving stock instance (Eq a, Eq v, Eq (f (Match a f v))) => Eq (MatchSubst a f v)
deriving stock instance (Show a, Show v, Show (f (Match a f v))) => Show (MatchSubst a f v)

-- | An opaque var id
-- Constructor exported for coercibility
newtype VarId = VarId { unVarId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

data PatGraph f v = PatGraph
  { pgRoot :: !VarId
  , pgNodes :: !(IntLikeMap VarId (PatF f v VarId))
  , pgVars :: !(HashMap v VarId)
  }

deriving stock instance (Eq v, Eq (f VarId)) => Eq (PatGraph f v)
deriving stock instance (Show v, Show (f VarId)) => Show (PatGraph f v)

type MatchC f v = (Traversable f, Eq v, Eq (f VarId), Hashable v, Hashable (f VarId))

data GraphState f v = GraphState
  { gsSrc :: !(Source VarId)
  , gsAssoc :: !(Assoc VarId (PatF f v VarId))
  }

emptyGraphState :: GraphState f v
emptyGraphState = GraphState (sourceNew (VarId 0)) assocNew

graphEnsurePart :: MatchC f v => PatF f v VarId -> State (GraphState f v) VarId
graphEnsurePart part = do
  mi <- gets (assocLookupByValue part . gsAssoc)
  case mi of
    Just i -> pure i
    Nothing -> state $ \st ->
      let (i, src') = sourceAddInc (gsSrc st)
          (_, assoc') = assocInsertInc i part (gsAssoc st)
      in (i, st { gsSrc = src', gsAssoc = assoc' })

graphEnsurePat :: MatchC f v => Pat f v -> State (GraphState f v) VarId
graphEnsurePat = cata go where
  go = \case
    FreePureF v -> graphEnsurePart (FreePureF v)
    FreeEmbedF fp -> do
      fi <- sequenceA fp
      graphEnsurePart (FreeEmbedF fi)

graphCanonicalize :: MatchC f v => GraphState f v -> IntLikeMap VarId (PatF f v VarId)
graphCanonicalize (GraphState _ assoc) =
  let fwd = assocFwd assoc
      equiv = assocEquiv assoc
  in fmap (fmap (`efLookupRoot` equiv)) fwd

patGraph :: MatchC f v => Pat f v -> PatGraph f v
patGraph p =
  let (i, st) = runState (graphEnsurePat p) emptyGraphState
      m = graphCanonicalize st
      n = HashMap.fromList (ILM.toList m >>= \(j, x) -> case x of { FreePureF v -> [(v, j)]; _ -> [] })
  in PatGraph i m n

data SolGraph a b f = SolGraph
  { sgByVar :: !(IntLikeMap VarId (IntLikeMap a (IntLikeSet b)))
  , sgByClass :: !(IntLikeMap a (IntLikeMap VarId (IntLikeSet b)))
  , sgNodes :: !(IntLikeMap b (f a))
  }

deriving stock instance (Eq a, Eq b, Eq (f a)) => Eq (SolGraph a b f)
deriving stock instance (Show a, Show b, Show (f a)) => Show (SolGraph a b f)

invertMM :: (Coercible x Int, Coercible y Int, Semigroup z) => IntLikeMap x (IntLikeMap y z) -> IntLikeMap y (IntLikeMap x z)
invertMM = foldl' goOuter ILM.empty . ILM.toList where
  goOuter m (x, myz) = foldl' (goInner x) m (ILM.toList myz)
  goInner x m (y, z) = ILM.alter (Just . maybe (ILM.singleton x z) (goMerge x z)) y m
  goMerge x z = ILM.alter (Just . maybe z (<> z)) x

solGraph :: (Functor f, Eq (f ()), Hashable (f ())) =>
  PatGraph f v -> EGraph d f -> SolGraph EClassId ENodeId f
solGraph pg eg =
  -- For each class, use footprint of reverse node assoc to find set of node ids
  let byVar = ILM.fromList $ ILM.toList (pgNodes pg) >>= \(i, pf) ->
        case pf of
          FreePureF _ -> []
          FreeEmbedF fi ->
            let fu = void fi
                cs = ILM.fromList $ ILM.toList (egClassMap eg) >>= \(c, inf) ->
                  let ns = eciNodes inf
                      fp = assocFootprint fu ns
                  in [(c, fp) | not (ILS.null fp)]
            in [(i, cs)]
      byClass = invertMM byVar
      nodes = assocFwd (egNodeAssoc eg)
  in SolGraph byVar byClass nodes

type SolStream a f v = Stream (Of (MatchSubst a f v)) Identity ()

nextSol :: SolStream a f v -> Maybe (MatchSubst a f v, SolStream a f v)
nextSol = runIdentity . S.uncons

allSols :: SolStream a f v -> [MatchSubst a f v]
allSols = runIdentity . S.toList_

solve :: PatGraph f v -> SolGraph a b f -> SolStream a f v
solve = error "TODO"

match :: (MatchC f v, Eq (f ()), Hashable (f ())) => Pat f v -> EGraph d f -> [MatchSubst EClassId f v]
match p eg =
  let pg = patGraph p
  in allSols (solve pg (solGraph pg eg))
