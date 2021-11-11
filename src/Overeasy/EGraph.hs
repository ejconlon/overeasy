{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An E-Graph implementation
module Overeasy.EGraph
  ( EClassId (..)
  , ENodeId (..)
  , EAnalysis (..)
  , EAnalysisOff (..)
  , EAnalysisAlgebra (..)
  , EClassInfo (..)
  , EGraph
  , WorkItem
  , WorkList
  , egSource
  , egUnionFind
  , egClassMap
  , egNodeAssoc
  , egHashCons
  , egWorkList
  , egClassSize
  , egTotalClassSize
  , egNodeSize
  , egFindNode
  , egFindTerm
  , egClassInfo
  , egNew
  , egClasses
  , egCanonicalize
  , egAddTerm
  , egMerge
  , egNeedsRebuild
  , egRebuild
  , egCanCompact
  , egCompact
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, get, gets, modify', state)
import Data.Foldable (for_)
import Data.Functor.Foldable (project)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocCanCompact, assocCompactInc, assocEnsure, assocLookupByValue, assocNew,
                       assocPartialLookupByKey, assocUpdate)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLike.Equiv (IntLikeEquiv)
import qualified Overeasy.IntLike.Equiv as ILE
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.MultiMap (IntLikeMultiMap)
import qualified Overeasy.IntLike.MultiMap as ILMM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (stateFold, stateLens)
import Overeasy.UnionFind (MergeManyRes (..), MergeRes (..), UnionFind, ufAdd, ufEquivRestricted, ufFind, ufMergeMany,
                           ufNew, ufPartialFind, ufRoots, ufSize, ufTotalSize)

-- | An opaque class id
newtype EClassId = EClassId { unEClassId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | An opaque node id
newtype ENodeId = ENodeId { unENodeId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | The definition of an 'EGraph' analysis.
-- Should satisfy `eaJoin q d [] == d`
class EAnalysis d f q | q -> d f where
  eaMake :: q -> f d -> d
  eaJoin :: q -> d -> [d] -> d

-- | A disabled analysis
data EAnalysisOff (f :: Type -> Type) = EAnalysisOff

instance EAnalysis () f (EAnalysisOff f) where
  eaMake _ _ = ()
  eaJoin _ _ _ = ()

newtype EAnalysisAlgebra d f = EAnalysisAlgebra
  { unEAnalysisAlgebra :: f d -> d
  }

instance Semigroup d => EAnalysis d f (EAnalysisAlgebra d f) where
  eaMake (EAnalysisAlgebra g) fd = g fd
  eaJoin _ d ds = sconcat (d :| ds)

data ENodeTriple d = ENodeTriple
  { entNode :: !ENodeId
  , entClass :: !EClassId
  , entData :: !d
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Info stored for every class: analysis data and class members.
data EClassInfo d = EClassInfo
  { eciData :: !d
  , eciNodes :: !(IntLikeSet ENodeId)
  , eciParents :: !(IntLikeSet ENodeId)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

type WorkItem = IntLikeSet EClassId
type WorkList = Seq WorkItem

-- private ctor
data EGraph d f = EGraph
  { egSource :: !(Source EClassId)
  , egUnionFind :: !(UnionFind EClassId)
  , egClassMap :: !(IntLikeMap EClassId (EClassInfo d))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(IntLikeMap ENodeId EClassId)
  , egWorkList :: !WorkList
  } deriving stock (Generic)

deriving stock instance (Eq d, Eq (f EClassId)) => Eq (EGraph d f)
deriving stock instance (Show d, Show (f EClassId)) => Show (EGraph d f)
deriving anyclass instance (NFData d, NFData (f EClassId)) => NFData (EGraph d f)

makeLensesFor
  [ ("egSource", "egSourceL")
  , ("egUnionFind", "egUnionFindL")
  , ("egClassMap", "egClassMapL")
  , ("egNodeAssoc", "egNodeAssocL")
  , ("egHashCons", "egHashConsL")
  , ("egWorkList", "egWorkListL")
  ] ''EGraph

-- | Number of equivalent classes in the 'EGraph' (see 'ufSize')
egClassSize :: EGraph d f -> Int
egClassSize = ufSize . egUnionFind

-- | Number of total classes in the 'EGraph' (see 'ufTotalSize')
egTotalClassSize :: EGraph d f -> Int
egTotalClassSize = ufTotalSize . egUnionFind

-- | Number of nodes in the 'EGraph'
egNodeSize :: EGraph d f -> Int
egNodeSize = ILM.size . egHashCons

-- | Lookup info for the given 'EClass'
egClassInfo :: EClassId -> EGraph d f -> Maybe (EClassInfo d)
egClassInfo c = ILM.lookup c . egClassMap

-- | Find the class of the given node, if it exists.
-- Note that you may have to canonicalize first to find it!
egFindNode :: (Eq (f EClassId), Hashable (f EClassId)) => f EClassId -> EGraph d f -> Maybe EClassId
egFindNode fc eg = do
  n <- assocLookupByValue fc (egNodeAssoc eg)
  ILM.lookup n (egHashCons eg)

-- | Find the class of the given term, if it exists
egFindTerm :: (RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => t -> EGraph d f -> Maybe EClassId
egFindTerm t eg = foldWholeM (`egFindNode` eg) t

-- | Creates a new 'EGraph'
egNew :: EGraph d f
egNew = EGraph (sourceNew (EClassId 0)) ufNew ILM.empty (assocNew (ENodeId 0)) ILM.empty Empty

-- | Yields all root classes
egClasses :: State (EGraph d f) (IntLikeSet EClassId)
egClasses = stateLens egUnionFindL ufRoots

-- | Find the canonical form of a node
egCanonicalize :: Traversable f => f EClassId -> State (EGraph d f) (Maybe (f EClassId))
egCanonicalize = stateLens egUnionFindL . fmap sequence . traverse ufFind

-- private
egCanonicalizeInternal :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => ENodeId -> State (EGraph d f) ENodeId
egCanonicalizeInternal x = do
  node <- stateLens egNodeAssocL (gets (assocPartialLookupByKey x))
  -- partial: guaranteed present by construction
  fz <- stateLens egUnionFindL (traverse ufPartialFind node)
  stateLens egNodeAssocL (assocUpdate x fz)

data AddNodeRes d = AddNodeRes !Changed !(Seq (ENodeTriple d))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup (AddNodeRes d) where
  AddNodeRes c1 p1 <> AddNodeRes c2 p2 = AddNodeRes (c1 <> c2) (p1 <> p2)

instance Monoid (AddNodeRes d) where
  mempty = AddNodeRes ChangedNo Seq.empty
  mappend = (<>)

-- private
egAddNodeSub :: (Show d, Show (f EClassId), EAnalysis d f q, Functor f, Eq (f EClassId), Hashable (f EClassId)) => q -> f (ENodeTriple d) -> State (EGraph d f) (Changed, ENodeTriple d)
egAddNodeSub q fcd = do
  let fc = fmap entClass fcd
  -- important: node should already be canonicalized!
  -- first lookup the node in the assoc to ensure uniqueness
  (c, n) <- stateLens egNodeAssocL (assocEnsure fc)
  traceM (unwords ["ADD NODE", "n=", show n, "fc=", show fc, "c=", show c])
  p <- case c of
    ChangedNo -> do
      -- node already exists; just return existing class id
      -- partial: should exist in hashcons by construction (next case)
      -- and should be mapped to the correct root class
      eg <- get
      let x = ILM.partialLookup n (egHashCons eg)
          d = eciData (ILM.partialLookup x (egClassMap eg))
      pure (ENodeTriple n x d)
    ChangedYes -> do
      -- node does not exist; get a new class id
      x <- stateLens egSourceL sourceAdd
      -- add it to the uf
      stateLens egUnionFindL (ufAdd x)
      -- map the node to the class id
      stateLens egHashConsL (modify' (ILM.insert n x))
      -- analyze the node and put that info in the class map
      let d = eaMake q (fmap entData fcd)
          i = EClassInfo d (ILS.singleton n) ILS.empty
      stateLens egClassMapL (modify' (ILM.insert x i))
      pure (ENodeTriple n x d)
  pure (c, p)

-- private
-- Similar in structure to foldWholeTrackM
egAddTermSub :: (Show d, Show (f EClassId), EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (AddNodeRes d, ENodeTriple d)
egAddTermSub q = go where
  go t = do
    -- unwrap to work with the functor layer
    let ft = project t
    -- add all child nodes
    frx <- traverse go ft
    -- collect info generated from child nodes and leave pure structure
    let (AddNodeRes changed1 children, fx) = sequenceA frx
    -- now fx should be canonicalized by construction
    -- add the node to get its node and class ids
    (changed2, z@(ENodeTriple n x _)) <- egAddNodeSub q fx
    -- now update all its children to add this as a parent
    for_ children $ \(ENodeTriple _ c _) ->
      stateLens egClassMapL (modify' (ILM.adjust (\v -> v { eciParents = ILS.insert n (eciParents v) }) c))
    pure (AddNodeRes (changed1 <> changed2) (Seq.singleton z), z)

-- | Adds a term (recursively) to the graph. If already in the graph, returns 'ChangedNo' and existing class id. Otherwise
-- returns 'ChangedYes' and a new class id.
egAddTerm :: (Show d, Show (f EClassId), EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (Changed, EClassId)
egAddTerm q t = fmap (\(AddNodeRes c _, ENodeTriple _ x _) -> (c, x)) (egAddTermSub q t)

-- | Merges two classes:
-- Returns 'Nothing' if the classes are not found
-- Otherwise returns the merged class id and whether anything has changed
-- If things have changed, then you must call 'egRebuild' before adding more terms.
-- (You can use 'egNeedsRebuild' to query this.)
egMerge :: EClassId -> EClassId -> State (EGraph d f) (Maybe Changed)
egMerge i j = egMergeMany (ILS.fromList [i, j])

egMergeMany :: IntLikeSet EClassId -> State (EGraph d f) (Maybe Changed)
egMergeMany cs = do
  traceM (unwords ["MERGE", "cs=", show cs])
  mayRoots <- stateLens egUnionFindL (traverse ufFind (ILS.toList cs))
  case sequence mayRoots of
    Nothing -> pure Nothing
    Just roots ->
      let rootsSet = ILS.fromList roots
      in if ILS.size rootsSet < 2
        then pure (Just ChangedNo)
        else do
          stateLens egWorkListL (modify' (:|> rootsSet))
          pure (Just ChangedYes)

-- | Have we merged classes and do we need to rebuild before adding more terms?
egNeedsRebuild :: EGraph d f -> Bool
egNeedsRebuild = not . null . egWorkList

-- private
-- Take the worklist (swapping for Empty).
egTakeWorklist :: State (EGraph d f) WorkList
egTakeWorklist = state $ \eg ->
  let wl = egWorkList eg
      eg' = case wl of { Empty -> eg; _ -> eg { egWorkList = Empty }}
  in (wl, eg')

-- private
egRebuildMerge :: WorkList -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeEquiv EClassId EClassId, IntLikeSet EClassId)
egRebuildMerge wl parents = stateLens egUnionFindL (go ILS.empty wl) where
  go !mems = \case
    Empty -> do
      mergeEquiv <- ufEquivRestricted (ILS.toList (parents <> mems))
      let roundOnly = ILS.map (`ILM.partialLookup` ILE.bwdView mergeEquiv) mems
      pure (mergeEquiv, roundOnly)
    cs :<| rest -> do
      mr <- ufMergeMany cs
      case mr of
        MergeManyResEmbed (MergeResChanged _) -> go (cs <> mems) rest
        _ -> go mems rest

-- private
-- TODO loop through merged classes instead of rewriting whole hashcons
egRebuildHashCons :: IntLikeEquiv EClassId EClassId -> State (EGraph d f) ()
egRebuildHashCons mergeEquiv = stateLens egHashConsL (modify' goHc) where
  bwd = ILE.bwdView mergeEquiv
  goHc = ILM.map (\x -> ILM.findWithDefault x x bwd)

-- private
egRebuildAssoc :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => IntLikeEquiv EClassId EClassId -> State (EGraph d f) (IntLikeSet EClassId, IntLikeMap ENodeId ENodeId, WorkList)
egRebuildAssoc mergeEquiv = do
  hc <- gets egHashCons
  -- For each class that we're going to merge
  stateFold (ILS.empty, ILM.empty, Empty) (ILM.keys (ILE.bwdView mergeEquiv)) $ \(ps, m, parentWl) c -> do
    traceM (unwords ["REBUILD ASSOC CLASS", "c=", show c])
    -- Get the class info
    eci <- gets (ILM.partialLookup c . egClassMap)
    -- Gather the parent classes for the next round
    let thisPs = ILS.map (`ILM.partialLookup` hc) (eciParents eci)
    traceM (unwords ["REBUILD ASSOC PARENTS", "c=", show c, "eciParents=", show (eciParents eci), "thisPs=", show thisPs])
    let ps' = thisPs <> ps
    -- For each node in the class
    (finalM, finalParentWl) <- stateFold (m, parentWl) (ILS.toList (eciNodes eci)) $ \(m', parentWl') n -> do
      -- Canonicalize it and add to the node map
      n' <- egCanonicalizeInternal n
      traceM (unwords ["REBUILD ASSOC NODE", "c=", show c, "n=", show n, "n'=", show n'])
      let m'' = ILM.insert n n' m'
      -- See if by canonicalizing we've induced a new equivalence
      let parentWl'' =
            if n == n'
              then parentWl'
              else
                let oldNodeClass = ILM.partialLookup n hc
                    newNodeClass = ILM.partialLookup n' hc
                in if oldNodeClass == newNodeClass then parentWl' else parentWl' :|> ILS.fromList [oldNodeClass, newNodeClass]
      pure (m'', parentWl'')
    pure (ps', finalM, finalParentWl)

-- private
egRebuildCanonWl :: IntLikeMultiMap ENodeId ENodeId -> State (EGraph d f) WorkList
egRebuildCanonWl nodeMultiMap = goRoot where
  goRoot = do
    hc <- gets egHashCons
    -- For each node in the new -> old multimap
    pure (foldr (goEach hc) Empty (ILMM.toList nodeMultiMap))
  goEach hc (_, oldNodes) ms =
    -- See what classes the nodes map to
    if ILS.size oldNodes > 1
      then
        -- Add to worklist if there are at least two classes for the same node
        let cs = ILS.map (`ILM.partialLookup` hc) oldNodes
        in if ILS.size cs > 1
          then ms :|> cs
          else ms
      else ms

-- private
egRebuildNodeRound :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => WorkList -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeSet EClassId, IntLikeMap ENodeId ENodeId, WorkList, IntLikeSet EClassId)
egRebuildNodeRound wl parents = do
  traceM (unwords ["#########################\n", "START ROUND", "wl=", show wl, "parents=", show parents])
  -- First merge all classes together and get merged class sets
  (mergeEquiv, roundOnly) <- egRebuildMerge wl parents
  traceM (unwords ["POST MERGE", "mergeEquiv=", show mergeEquiv, "roundOnly=", show roundOnly])
  uf <- gets egUnionFind
  traceM (unwords ["POST UF", "uf=", show uf])
  -- Now update the hashcons so node ids point to merged classes
  egRebuildHashCons mergeEquiv
  hc <- gets egHashCons
  traceM (unwords ["POST HASHCONS", "hc=", show hc])
  -- Traverse all classes and canonicalize their nodes,
  -- recording the mapping from old -> new
  -- Also track all possible parent classes
  (candParents, nodeMap, parentWl) <- egRebuildAssoc mergeEquiv
  traceM (unwords ["POST ASSOC", "candParents=", show candParents, "nodeMap=", show nodeMap, "parentWl=", show parentWl])
  -- Invert the node map to find new equivalences
  -- This produces a multimap of new -> set of old
  let nodeMultiMap = ILMM.fromInvertedMap nodeMap
  -- Find new equivalences induced by canonicalization
  canonWl <- egRebuildCanonWl nodeMultiMap
  -- Track new class ids for final class rebuilding
  let touchedClasses = ILS.fromList (ILM.keys (ILE.bwdView mergeEquiv))
  -- And track next worklist
  let finalWl = parentWl <> canonWl
  -- And track parent classes for next round
  let finalParents = ILS.filter (not . (`ILS.member` touchedClasses)) candParents  -- TODO roundOnly instead of touchedClasses?
  traceM (unwords ["END ROUND", "touchedClasses=", show touchedClasses, "nodeMap=", show nodeMap, "canonWl=", show canonWl, "finalParents=", show finalParents])
  pure (touchedClasses, nodeMap, finalWl, finalParents)

egRebuildClassSingle :: EAnalysis d f q => q -> IntLikeMap EClassId EClassId -> IntLikeMap ENodeId ENodeId -> (EClassId, IntLikeSet EClassId) -> IntLikeMap EClassId (EClassInfo d) -> IntLikeMap EClassId (EClassInfo d)
egRebuildClassSingle q _ nodeMap (newClass, oldClasses) initCm =
  let EClassInfo rootData rootNodes rootParents = ILM.partialLookup newClass initCm
      finalData = eaJoin q rootData (fmap (\c -> eciData (ILM.partialLookup c initCm)) (ILS.toList oldClasses))
      lookupClassNodes c = eciNodes (ILM.partialLookup c initCm)
      lookupClassParents c = eciParents (ILM.partialLookup c initCm)
      allClassParents = foldr (\c s -> lookupClassParents c <> s) rootParents (ILS.toList oldClasses)
      finalNodes = foldr (\c s -> lookupClassNodes c <> s) rootNodes (ILS.toList oldClasses)
      finalParents = ILS.fromList $ ILS.toList allClassParents >>= \pn -> [pn | not (ILS.member pn finalNodes)]
      finalInfo = EClassInfo finalData finalNodes finalParents
      finalCm = ILM.insert newClass finalInfo (foldr ILM.delete initCm (ILS.toList oldClasses))
  in finalCm

-- private
-- Rebuilds the classmap: merges old class infos into root class infos
-- Returns list of modified root classes
egRebuildClassMap :: EAnalysis d f q => q -> IntLikeSet EClassId -> IntLikeMap ENodeId ENodeId -> State (EGraph d f) (IntLikeSet EClassId)
egRebuildClassMap q touchedClasses nodeMap = do
  -- Calculate final merged class sets
  mergeEquiv <- stateLens egUnionFindL (ufEquivRestricted (ILS.toList touchedClasses))
  -- For each class set, rebuild the root class and update the class map
  stateLens egClassMapL $ modify' $ \initCm ->
    foldr (egRebuildClassSingle q (ILE.bwdView mergeEquiv) nodeMap) initCm (ILMM.toList (ILE.fwdView mergeEquiv))
  pure (ILS.fromList (ILM.keys (ILE.fwdView mergeEquiv)))

-- | Rebuilds the 'EGraph' after merging to allow adding more terms. (Always safe to call.)
egRebuild :: (EAnalysis d f q, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> State (EGraph d f) (IntLikeSet EClassId)
egRebuild q = goRec where
  goRec = do
    -- Read and clear the worklist - from now on nothing should add to it
    wl <- egTakeWorklist
    -- Merge and induce equivalences
    (tc, nm) <- goNodeRounds ILS.empty ILM.empty wl ILS.empty
    -- Now everything is merged so we only have to rewrite the changed parts of the classmap
    egRebuildClassMap q tc nm
  goNodeRounds !tc !nm !wl !parents =
    if null wl && ILS.null parents
      then pure (tc, nm)
      else do
        (newTc, newNm, newWl, newParents) <- egRebuildNodeRound wl parents
        let mergedTc = newTc <> tc
            mergedNm = newNm <> nm  -- NB keep new on the left!
        goNodeRounds mergedTc mergedNm newWl newParents

egCanCompact :: EGraph d f -> Bool
egCanCompact = assocCanCompact . egNodeAssoc

egCompactInc :: (Eq (f EClassId), Hashable (f EClassId)) => EGraph d f -> EGraph d f
egCompactInc eg =
  let assoc = egNodeAssoc eg
  in if assocCanCompact assoc
    then
      -- TODO take assocDeadFwd as non-canonical nodes
      -- remove them from classmap nodes, hashcons, and unionfind
      let assoc' = assocCompactInc assoc
      in eg { egNodeAssoc = assoc'}
    else eg

egCompact :: (Eq (f EClassId), Hashable (f EClassId)) => State (EGraph d f) ()
egCompact = modify' egCompactInc
