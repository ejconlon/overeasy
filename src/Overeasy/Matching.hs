{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Methods to match patterns in 'EGraph's (aka e-matching)
module Overeasy.Matching
  ( Pat
  , patVars
  , Subst
  , substVars
  , VarId (..)
  , PatGraphC
  , PatGraph (..)
  , patGraph
  , Match (..)
  , MatchPat (..)
  , MatchF (..)
  , MatchPatF (..)
  , matchVars
  , matchClasses
  , MatchSubst (..)
  , SolGraphC
  , SolGraph (..)
  , solGraph
  , SolveC
  , solve
  , match
  ) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (MonadState (..), State, evalState, gets, modify', runState)
import Data.Coerce (Coercible)
import Data.Foldable (fold, foldMap')
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
import Overeasy.Assoc (Assoc, assocBwd, assocEquiv, assocFootprint, assocFwd, assocInsertInc, assocLookupByValue,
                       assocNew)
import Overeasy.EGraph (EClassId (..), EGraph (egHashCons), ENodeId (..), eciNodes, egClassMap, egNodeAssoc)
import Overeasy.EquivFind (efLookupRoot)
import Overeasy.Source (Source, sourceAddInc, sourceNew)
import Overeasy.Streams (Stream, chooseWith, streamAll)
import Unfree (Free, FreeF (..))

-- | A pattern is exactly the free monad over the expression functor
-- It has spots for var names ('FreePure') and spots for structural
-- pieces ('FreeEmbed')
type Pat = Free

type PatF = FreeF

patVars :: (Foldable f, Eq v, Hashable v) => Pat f v -> HashSet v
patVars = foldMap' HashSet.singleton

type Subst c v = HashMap v c

substVars :: Subst a v -> HashSet v
substVars = HashMap.keysSet

-- | A match is a pattern annotated with classes
data Match c f v = Match
  { matchClass :: !c
  , matchPat :: !(MatchPat c f v)
  } deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq c, Eq v, Eq (f (Match c f v))) => Eq (Match c f v)
deriving stock instance (Show c, Show v, Show (f (Match c f v))) => Show (Match c f v)

data MatchPat c f v =
    MatchPatPure !v
  | MatchPatEmbed !(f (Match c f v))
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq v, Eq (f (Match c f v))) => Eq (MatchPat c f v)
deriving stock instance (Show v, Show (f (Match c f v))) => Show (MatchPat c f v)

-- | The base functor of 'Match'
data MatchF c f v r = MatchF
  { matchClassF :: !c
  , matchPatF :: !(MatchPatF f v r)
  } deriving stock (Functor, Foldable, Traversable)

data MatchPatF f v r =
    MatchPatPureF !v
  | MatchPatEmbedF !(f r)
  deriving stock (Functor, Foldable, Traversable)

type instance Base (Match c f v) = MatchF c f v

instance Functor f => Recursive (Match c f v) where
  project (Match cl mp) = MatchF cl $ case mp of
     MatchPatPure v -> MatchPatPureF v
     MatchPatEmbed f -> MatchPatEmbedF f

instance Functor f => Corecursive (Match c f v) where
  embed (MatchF cl mpf) = Match cl $ case mpf of
     MatchPatPureF v -> MatchPatPure v
     MatchPatEmbedF f -> MatchPatEmbed f

matchVars :: (Foldable f, Eq v, Hashable v) => Match c f v -> HashSet v
matchVars = foldMap' HashSet.singleton

matchClasses :: (Coercible c Int, Functor f, Foldable f) => Match c f v -> IntLikeSet c
matchClasses = cata go where
  go (MatchF cl mpf) = ILS.insert cl $ case mpf of
    MatchPatPureF _ -> ILS.empty
    MatchPatEmbedF fc -> fold fc

data MatchSubst c f v = MatchSubst
  { msMatch :: !(Match c f v)
  , msSubst :: !(Subst c v)
  }

deriving stock instance (Eq c, Eq v, Eq (f (Match c f v))) => Eq (MatchSubst c f v)
deriving stock instance (Show c, Show v, Show (f (Match c f v))) => Show (MatchSubst c f v)

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

type PatGraphC f v = (Traversable f, Eq v, Eq (f VarId), Hashable v, Hashable (f VarId))

data GraphState f v = GraphState
  { gsSrc :: !(Source VarId)
  , gsAssoc :: !(Assoc VarId (PatF f v VarId))
  }

emptyGraphState :: GraphState f v
emptyGraphState = GraphState (sourceNew (VarId 0)) assocNew

graphEnsurePart :: PatGraphC f v => PatF f v VarId -> State (GraphState f v) VarId
graphEnsurePart part = do
  mi <- gets (assocLookupByValue part . gsAssoc)
  case mi of
    Just i -> pure i
    Nothing -> state $ \st ->
      let (i, src') = sourceAddInc (gsSrc st)
          (_, assoc') = assocInsertInc i part (gsAssoc st)
      in (i, st { gsSrc = src', gsAssoc = assoc' })

graphEnsurePat :: PatGraphC f v => Pat f v -> State (GraphState f v) VarId
graphEnsurePat = cata go where
  go = \case
    FreePureF v -> graphEnsurePart (FreePureF v)
    FreeEmbedF fp -> do
      fi <- sequenceA fp
      graphEnsurePart (FreeEmbedF fi)

graphCanonicalize :: PatGraphC f v => GraphState f v -> IntLikeMap VarId (PatF f v VarId)
graphCanonicalize (GraphState _ assoc) =
  let fwd = assocFwd assoc
      equiv = assocEquiv assoc
  in fmap (fmap (`efLookupRoot` equiv)) fwd

patGraph :: PatGraphC f v => Pat f v -> PatGraph f v
patGraph p =
  let (i, st) = runState (graphEnsurePat p) emptyGraphState
      m = graphCanonicalize st
      n = HashMap.fromList (ILM.toList m >>= \(j, x) -> case x of { FreePureF v -> [(v, j)]; _ -> [] })
  in PatGraph i m n

data SolGraph c f = SolGraph
  { sgByVar :: !(IntLikeMap VarId (IntLikeSet c)) -- (IntLikeMap a (IntLikeSet b)))
  -- Map of var -> class -> nodes
  -- Contains all vars represented as embedded patterns.
  -- If the inner map is empty, that means the pattern was not matched.
  -- The inner set will not be empty.
  -- , sgByClass :: !(IntLikeMap a (IntLikeMap VarId (IntLikeSet b)))
  -- Map of class -> var -> nodes, the inversion of 'sgByVar'.
  -- Only contains classes that have pattern matches.
  -- Neither the inner map nor the inner set will be empty.
  -- , sgNodes :: !(Assoc b (f a))
  , sgNodes :: !(HashMap (f c) c)
  }

deriving stock instance (Eq c, Eq (f c)) => Eq (SolGraph c f)
deriving stock instance (Show c, Show (f c)) => Show (SolGraph c f)

-- invertMM :: (Coercible x Int, Coercible y Int, Semigroup z) => IntLikeMap x (IntLikeMap y z) -> IntLikeMap y (IntLikeMap x z)
-- invertMM = foldl' goOuter ILM.empty . ILM.toList where
--   goOuter m (x, myz) = foldl' (goInner x) m (ILM.toList myz)
--   goInner x m (y, z) = ILM.alter (Just . maybe (ILM.singleton x z) (goMerge x z)) y m
--   goMerge x z = ILM.alter (Just . maybe z (<> z)) x

type SolGraphC f = (Functor f, Eq (f ()), Hashable (f ()))

solGraph :: SolGraphC f => PatGraph f v -> EGraph d f -> SolGraph EClassId f
solGraph pg eg =
  -- For each class, use footprint of reverse node assoc to find set of node ids
  let byVar = ILM.fromList $ ILM.toList (pgNodes pg) >>= \(i, pf) ->
        case pf of
          FreePureF _ -> []
          FreeEmbedF fi ->
            let fu = void fi
                cs = ILM.toList (egClassMap eg) >>= \(c, inf) ->
                  let ns = eciNodes inf
                      fp = assocFootprint fu ns
                  in [c | not (ILS.null fp)]
            in [(i, ILS.fromList cs)]
      hc = egHashCons eg
      nodes = fmap (`ILM.partialLookup` hc) (assocBwd (egNodeAssoc eg))
  in SolGraph byVar nodes

data SolEnv c f v = SolEnv
  { sePatGraph :: !(PatGraph f v)
  , seSolGraph :: !(SolGraph c f)
  }

newtype SolState c = SolState
  { ssClasses :: IntLikeMap VarId c
  } deriving (Eq, Show)

type SolStream c f v z = Stream (SolEnv c f v) (SolState c) z

type SolveC c f v = (Traversable f, Coercible c Int, Eq v, Hashable v, Eq (f c), Hashable (f c))

constructMatch :: Traversable f => IntLikeMap VarId (PatF f v VarId) -> IntLikeMap VarId c -> VarId -> Match c f v
constructMatch nodes classes i0 = evalState (go i0) ILM.empty where
  go i = do
    cache <- get
    case ILM.lookup i cache of
      Just res -> pure res
      Nothing -> do
        let c = ILM.partialLookup i classes
        mp <- case ILM.partialLookup i nodes of
          FreePureF v -> pure $! MatchPatPure v
          FreeEmbedF f -> fmap MatchPatEmbed (traverse go f)
        pure $! Match c mp

constructSubst :: HashMap v VarId -> IntLikeMap VarId a -> Subst a v
constructSubst vars classes = fmap (`ILM.partialLookup` classes) vars

solveYield :: Traversable f => SolStream c f v (MatchSubst c f v)
solveYield = do
  pg <- asks sePatGraph
  classes <- gets ssClasses
  let mat = constructMatch (pgNodes pg) classes (pgRoot pg)
      subst = constructSubst (pgVars pg) classes
      ms = MatchSubst mat subst
  pure ms

solve :: SolveC c f v => SolStream c f v (MatchSubst c f v)
solve = do
  i <- asks (pgRoot . sePatGraph)
  void (solveRec i)
  solveYield

solveChoose :: SolveC c f v => VarId -> IntLikeSet c -> SolStream c f v c
solveChoose i cs = chooseWith (ILS.toList cs) (solveSet i)

solveSet :: VarId -> c -> SolStream c f v c
solveSet i c =
  c <$ modify' (\ss -> ss { ssClasses = ILM.insert i c (ssClasses ss) })

solveRec :: SolveC c f v => VarId -> SolStream c f v c
solveRec i = do
  ms <- gets (ILM.lookup i . ssClasses)
  case ms of
    -- Seen before, return solution
    Just s -> pure s
    -- Unseen
    Nothing -> do
      n <- asks (ILM.partialLookup i . pgNodes . sePatGraph)
      case n of
        -- Free var, choose a solution for each class in `sgByVar i`
        FreePureF _ -> do
          cs <- asks (ILM.partialLookup i . sgByVar . seSolGraph)
          solveChoose i cs
        -- Embedded functor, traverse and emit solution if present
        FreeEmbedF fi -> do
          fa <- traverse solveRec fi
          mc <- asks (HashMap.lookup fa . sgNodes . seSolGraph)
          case mc of
            Nothing -> empty
            Just c -> solveSet i c

match :: (PatGraphC f v, SolGraphC f, SolveC EClassId f v) => Pat f v -> EGraph d f -> [MatchSubst EClassId f v]
match p eg =
  let pg = patGraph p
      sg = solGraph pg eg
  in if any ILS.null (ILM.elems (sgByVar sg))
    -- If any var id has no patches, the pattern won't match, so don't try to solve
    then []
    else streamAll solve (SolEnv pg sg) (SolState ILM.empty)
