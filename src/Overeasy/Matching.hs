{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Methods to match patterns in 'EGraph's (aka e-matching)
module Overeasy.Matching
  ( Pat
  , patVars
  , PatGroup
  , Subst
  , substVars
  , VarId (..)
  , PatGraphC
  , PatGraph (..)
  , patGraph
  , singlePatGraph
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
  , SolStream
  , SolveC
  , solve
  , match
  )
where

import Bowtie.Free (Free, FreeF (..))
import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (MonadState (..), State, evalState, execState, gets, modify', runState)
import Data.Bifunctor (bimap)
import Data.Coerce (Coercible)
import Data.Foldable (fold, foldMap', foldl', for_, toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Overeasy.Assoc
  ( Assoc
  , assocBwd
  , assocEquiv
  , assocFootprint
  , assocFwd
  , assocInsertInc
  , assocLookupByValue
  , assocNew
  )
import Overeasy.EGraph (EClassId (..), EGraph (egHashCons), ENodeId (..), eciNodes, egClassMap, egNodeAssoc)
import Overeasy.EquivFind (efLookupRoot)
import Overeasy.Source (Source, sourceAddInc, sourceNew)
import Overeasy.Streams (Stream, chooseWith, streamAll)

-- | A pattern is exactly the free monad over the expression functor
-- It has spots for var names ('FreePure') and spots for structural
-- pieces ('FreeEmbed')
type Pat = Free

-- | The base functor of 'Pat'.
type PatF = FreeF

-- | The set of vars for a pattern.
patVars :: (Foldable f, Ord v) => Pat f v -> Set v
patVars = foldMap' Set.singleton

-- | A pattern group is some container with patterns inside.
-- This container might be 'Identity' for one pattern, or a list or
-- map for many patterns.
type PatGroup g f v = g (Pat f v)

-- | A substitution.
type Subst c v = Map v c

-- | The set of vars for a substitution.
substVars :: Subst a v -> Set v
substVars = Map.keysSet

-- | A match is a pattern annotated with classes (or other data).
data Match c f v = Match
  { matchAnno :: !c
  , matchPat :: !(MatchPat c f v)
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq c, Eq v, Eq (f (Match c f v))) => Eq (Match c f v)

deriving stock instance (Show c, Show v, Show (f (Match c f v))) => Show (Match c f v)

-- | Tie the knot - the inner layer of a match.
data MatchPat c f v
  = MatchPatPure !v
  | MatchPatEmbed !(f (Match c f v))
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq v, Eq (f (Match c f v))) => Eq (MatchPat c f v)

deriving stock instance (Show v, Show (f (Match c f v))) => Show (MatchPat c f v)

-- | The base functor of 'Match'
data MatchF c f v r = MatchF
  { matchClassF :: !c
  , matchPatF :: !(MatchPatF f v r)
  }
  deriving stock (Functor, Foldable, Traversable)

-- | Tie the knot - the inner part of 'MatchF'.
data MatchPatF f v r
  = MatchPatPureF !v
  | MatchPatEmbedF !(f r)
  deriving stock (Functor, Foldable, Traversable)

type instance Base (Match c f v) = MatchF c f v

instance (Functor f) => Recursive (Match c f v) where
  project (Match cl mp) = MatchF cl $ case mp of
    MatchPatPure v -> MatchPatPureF v
    MatchPatEmbed f -> MatchPatEmbedF f

instance (Functor f) => Corecursive (Match c f v) where
  embed (MatchF cl mpf) = Match cl $ case mpf of
    MatchPatPureF v -> MatchPatPure v
    MatchPatEmbedF f -> MatchPatEmbed f

-- | The set of vars in a match.
matchVars :: (Foldable f, Ord v) => Match c f v -> Set v
matchVars = foldMap' Set.singleton

-- | The set of classes in a match.
matchClasses :: (Coercible c Int, Functor f, Foldable f) => Match c f v -> IntLikeSet c
matchClasses = cata go
 where
  go (MatchF cl mpf) = ILS.insert cl $ case mpf of
    MatchPatPureF _ -> ILS.empty
    MatchPatEmbedF fc -> fold fc

-- | A apri of match and substitution.
data MatchSubst c f v = MatchSubst
  { msMatch :: !(Match c f v)
  , msSubst :: !(Subst c v)
  }

deriving stock instance (Eq c, Eq v, Eq (f (Match c f v))) => Eq (MatchSubst c f v)

deriving stock instance (Show c, Show v, Show (f (Match c f v))) => Show (MatchSubst c f v)

-- | An opaque var id
-- Constructor exported for coercibility
newtype VarId = VarId {unVarId :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, NFData)

-- | A pattern graph - can be created once for each pattern and reused
-- for many iterations of search.
-- 'g' is the pattern group functor.
-- 'f' is the language functor.
data PatGraph g f v = PatGraph
  { pgRoots :: !(g VarId)
  , pgNodes :: !(IntLikeMap VarId (PatF f v VarId))
  , pgVars :: !(Map v VarId)
  }

deriving stock instance (Eq v, Eq (g VarId), Eq (f VarId)) => Eq (PatGraph g f v)

deriving stock instance (Show v, Show (g VarId), Show (f VarId)) => Show (PatGraph g f v)

-- | The set of constraints necessary to build a pattern graph.
type PatGraphC f v = (Traversable f, Traversable f, Ord v, Ord (f VarId))

data GraphState f v = GraphState
  { gsSrc :: !(Source VarId)
  , gsAssoc :: !(Assoc VarId (PatF f v VarId))
  }

emptyGraphState :: GraphState f v
emptyGraphState = GraphState (sourceNew (VarId 0)) assocNew

graphEnsurePart :: (PatGraphC f v) => PatF f v VarId -> State (GraphState f v) VarId
graphEnsurePart part = do
  mi <- gets (assocLookupByValue part . gsAssoc)
  case mi of
    Just i -> pure i
    Nothing -> state $ \st ->
      let (i, src') = sourceAddInc (gsSrc st)
          (_, assoc') = assocInsertInc i part (gsAssoc st)
      in  (i, st {gsSrc = src', gsAssoc = assoc'})

graphEnsurePat :: (PatGraphC f v) => Pat f v -> State (GraphState f v) VarId
graphEnsurePat = cata go
 where
  go = \case
    FreePureF v -> graphEnsurePart (FreePureF v)
    FreeEmbedF fp -> do
      fi <- sequenceA fp
      graphEnsurePart (FreeEmbedF fi)

graphCanonicalize :: (PatGraphC f v) => GraphState f v -> IntLikeMap VarId (PatF f v VarId)
graphCanonicalize (GraphState _ assoc) =
  let fwd = assocFwd assoc
      equiv = assocEquiv assoc
  in  fmap (fmap (`efLookupRoot` equiv)) fwd

-- | Builds a pattern graph from a group of patterns.
patGraph :: (Traversable g, PatGraphC f v) => PatGroup g f v -> PatGraph g f v
patGraph ps =
  let (gr, st) = runState (traverse graphEnsurePat ps) emptyGraphState
      m = graphCanonicalize st
      n = Map.fromList (ILM.toList m >>= \(j, x) -> case x of FreePureF v -> [(v, j)]; _ -> [])
  in  PatGraph gr m n

-- | Builds a pattern graph from a single pattern.
-- If you have more than one pattern, building the group all at once is going to
-- make finding solutions more efficient.
singlePatGraph :: (PatGraphC f v) => Pat f v -> PatGraph Identity f v
singlePatGraph = patGraph . Identity

-- | A solution graph - must be created from an e-graph each merge/rebuild.
data SolGraph c f = SolGraph
  { sgByVar :: !(IntLikeMap VarId (IntLikeSet c))
  -- ^ Map of var -> classes.
  -- Contains all vars.
  -- If the inner map is empty, that means the pattern was not matched.
  -- The inner set will not be empty.
  , sgNodes :: !(Map (f c) c)
  -- ^ Map of node structures to classes.
  }

deriving stock instance (Eq c, Eq (f c)) => Eq (SolGraph c f)

deriving stock instance (Show c, Show (f c)) => Show (SolGraph c f)

-- | The set of constraints necessary to build a solution graph.
type SolGraphC f = (Functor f, Foldable f, Ord (f ()))

-- | Builds a solution graph from an e-graph.
solGraph :: (SolGraphC f) => PatGraph g f v -> EGraph d f -> SolGraph EClassId f
solGraph pg eg =
  -- For each class, use footprint of reverse node assoc to find set of node ids
  -- Start with just the embedded nodes
  let byVarEmbed =
        ILM.fromList $
          ILM.toList (pgNodes pg) >>= \(i, pf) ->
            case pf of
              FreePureF _ -> []
              FreeEmbedF fi ->
                let fu = void fi
                    cns =
                      ILM.toList (egClassMap eg) >>= \(c, inf) ->
                        let ns = eciNodes inf
                            fp = assocFootprint fu ns
                        in  [(c, fp) | not (ILS.null fp)]
                in  [(i, bimap ILS.fromList mconcat (unzip cns))]
      byVar = genByVar byVarEmbed (pgNodes pg) (assocFwd (egNodeAssoc eg))
      hc = egHashCons eg
      nodes = fmap (`ILM.partialLookup` hc) (assocBwd (egNodeAssoc eg))
  in  SolGraph byVar nodes

data Record
  = RecordPure !VarId !(IntLikeSet EClassId)
  | RecordEmbed
  deriving stock (Eq, Show)

type Records = [Record]

initRecords :: (Foldable f) => IntLikeMap VarId (PatF f v VarId) -> f VarId -> Records
initRecords nodes = fmap (\i -> case ILM.partialLookup i nodes of FreePureF _ -> RecordPure i ILS.empty; _ -> RecordEmbed) . toList

updateRecords :: (Foldable f) => Records -> f EClassId -> Records
updateRecords rs = zipWith (\r c -> case r of RecordPure v cs -> RecordPure v (ILS.insert c cs); _ -> r) rs . toList

genByVar
  :: (Foldable f)
  => IntLikeMap VarId (IntLikeSet EClassId, IntLikeSet ENodeId)
  -> IntLikeMap VarId (PatF f v VarId)
  -> IntLikeMap ENodeId (f EClassId)
  -> IntLikeMap VarId (IntLikeSet EClassId)
genByVar byVarEmbed nodes fwd = execState (for_ (ILM.toList nodes) go) (fmap fst byVarEmbed)
 where
  go (i, pf) =
    case pf of
      FreePureF _ -> pure ()
      FreeEmbedF fi -> do
        -- We've gone through embedded patterns before so we're able
        -- to look up nodes for each pattern
        let (_, ns) = ILM.partialLookup i byVarEmbed
        -- For each node, update positionally what it could be
        let rs = foldl' (\rsx n -> let fc = ILM.partialLookup n fwd in updateRecords rsx fc) (initRecords nodes fi) (ILS.toList ns)
        -- Finally update the map; if missing set the positions as is, otherwise take intersection
        modify' $ \m -> foldl' (\mx r -> case r of RecordPure j cs -> ILM.alter (Just . maybe cs (ILS.intersection cs)) j mx; _ -> mx) m rs

data SolEnv c g f v = SolEnv
  { sePatGraph :: !(PatGraph g f v)
  , seSolGraph :: !(SolGraph c f)
  }

newtype SolState c = SolState
  { ssClasses :: IntLikeMap VarId c
  }
  deriving (Eq, Show)

-- | A stream of solutions. Can be demanded all at once or interleaved.
type SolStream c g f v z = Stream (SolEnv c g f v) (SolState c) z

-- | The set of constraints necessary to search for solutions.
type SolveC c f v = (Traversable f, Coercible c Int, Ord v, Ord (f c))

constructMatch :: (Traversable f) => IntLikeMap VarId (PatF f v VarId) -> IntLikeMap VarId c -> VarId -> Match c f v
constructMatch nodes classes i0 = evalState (go i0) ILM.empty
 where
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

constructSubst :: Map v VarId -> IntLikeMap VarId a -> Subst a v
constructSubst vars classes = fmap (`ILM.partialLookup` classes) vars

solveYield :: (Traversable f) => VarId -> SolStream c g f v (MatchSubst c f v)
solveYield i = do
  pg <- asks sePatGraph
  classes <- gets ssClasses
  let mat = constructMatch (pgNodes pg) classes i
      subst = constructSubst (pgVars pg) classes
      ms = MatchSubst mat subst
  pure ms

-- | Produces a stream of solutions (e-matches).
solve :: (Foldable g, SolveC c f v) => SolStream c g f v (MatchSubst c f v)
solve = do
  roots <- asks (pgRoots . sePatGraph)
  chooseWith roots $ \i -> do
    void (solveRec i)
    solveYield i

solveChoose :: (SolveC c f v) => VarId -> IntLikeSet c -> SolStream c g f v c
solveChoose i cs = chooseWith (ILS.toList cs) (solveSet i)

solveSet :: VarId -> c -> SolStream c g f v c
solveSet i c =
  c <$ modify' (\ss -> ss {ssClasses = ILM.insert i c (ssClasses ss)})

solveRec :: (SolveC c f v) => VarId -> SolStream c g f v c
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
          mc <- asks (Map.lookup fa . sgNodes . seSolGraph)
          case mc of
            Nothing -> empty
            Just c -> solveSet i c

-- | The easiest way to do e-matching: given a pattern and an e-graph, yield the list of matches.
-- Note that it's more efficient to keep a 'PatGraph' and use the same solution graph for multiple patterns.)
match :: (PatGraphC f v, SolGraphC f, SolveC EClassId f v) => Pat f v -> EGraph d f -> [MatchSubst EClassId f v]
match p eg =
  let pg = singlePatGraph p
      sg = solGraph pg eg
  in  if any ILS.null (ILM.elems (sgByVar sg))
        then -- If any var id has no patches, the pattern won't match, so don't try to solve
          []
        else streamAll solve (SolEnv pg sg) (SolState ILM.empty)
