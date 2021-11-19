{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- -- | An E-Graph implementation
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
  , egClassSource
  , egNodeSource
  , egEquivFind
  , egClassMap
  , egDeadClasses
  , egNodeAssoc
  , egHashCons
  , egWorkList
  , egClassSize
  , egNodeSize
  , egFindNode
  , egFindTerm
  , egClassInfo
  , egNew
  , egClasses
  , egCanonicalize
  , egAddTerm
  , egMerge
  , egMergeMany
  , egNeedsRebuild
  , egRebuild
  , egCanCompact
  , egCompact
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, get, gets, modify', state)
import Data.Foldable (foldl', for_, toList)
import Data.Functor.Foldable (project)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
-- import Debug.Trace (traceM)
import GHC.Generics (Generic)
-- import Lens.Micro.TH (makeLensesFor)
-- import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocCanCompact, assocCompactInc, assocFwd, assocLookupByValue, assocNew, assocInsertInc, assocPartialLookupByKey, assocDead)
import Overeasy.Classes (Changed (..))
import Overeasy.EquivFind (EquivFind, EquivMergeSetsRes (..), efAdd, efClosure, efCompactInc, efFindRoot,
                           efLookupLeaves, efLookupRoot, efMergeSetsInc, efNew, efRoots, efRootsSize, efAddInc)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.MultiMap (IntLikeMultiMap)
import qualified Overeasy.IntLike.MultiMap as ILMM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAdd, sourceNew, sourceAddInc)
import Overeasy.StateUtil (stateFold)
import Control.Monad (unless)

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
-- TODO Allow these to signal additional information
-- so we can catch weird merges and so on
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
  { egClassSource :: !(Source EClassId)
  , egNodeSource :: !(Source ENodeId)
  , egEquivFind :: !(EquivFind EClassId)
  , egClassMap :: !(IntLikeMap EClassId (EClassInfo d))
  , egDeadClasses :: !(IntLikeSet EClassId)
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(IntLikeMap ENodeId EClassId)
  , egWorkList :: !WorkList
  } deriving stock (Generic)

deriving stock instance (Eq d, Eq (f EClassId)) => Eq (EGraph d f)
deriving stock instance (Show d, Show (f EClassId)) => Show (EGraph d f)
deriving anyclass instance (NFData d, NFData (f EClassId)) => NFData (EGraph d f)

-- makeLensesFor
--   [ ("egClassSource", "egClassSourceL")
--   , ("egNodeSource", "egNodeSourceL")
--   , ("egEquivFind", "egEquivFindL")
--   , ("egClassMap", "egClassMapL")
--   , ("egDeadClasses", "egDeadClassesL")
--   , ("egNodeAssoc", "egNodeAssocL")
--   , ("egHashCons", "egHashConsL")
--   , ("egWorkList", "egWorkListL")
--   ] ''EGraph

-- | Number of equivalent classes in the 'EGraph' (see 'ufSize')
egClassSize :: EGraph d f -> Int
egClassSize = efRootsSize . egEquivFind

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
egNew = EGraph (sourceNew (EClassId 0)) (sourceNew (ENodeId 0)) efNew ILM.empty ILS.empty assocNew ILM.empty Empty

-- | Yields all root classes
egClasses :: State (EGraph d f) [EClassId]
egClasses = gets (efRoots . egEquivFind)

-- | Find the canonical form of a node
egCanonicalize :: Traversable f => f EClassId -> State (EGraph d f) (Maybe (f EClassId))
egCanonicalize fc = fmap (\ef -> traverse (`efFindRoot` ef) fc) (gets egEquivFind)

-- private
egCanonicalizeInternal :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => ENodeId -> State (EGraph d f) ENodeId
egCanonicalizeInternal x = state $ \eg ->
  let ef = egEquivFind eg
      assoc = egNodeAssoc eg
      node = assocPartialLookupByKey x assoc
      fz = fmap (`efLookupRoot` ef) node
      (y, massoc) = assocInsertInc x fz assoc
  in (y, maybe eg (\assoc' -> eg { egNodeAssoc = assoc' }) massoc)

data AddNodeRes d = AddNodeRes !Changed !(Seq (ENodeTriple d))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup (AddNodeRes d) where
  AddNodeRes c1 p1 <> AddNodeRes c2 p2 = AddNodeRes (c1 <> c2) (p1 <> p2)

instance Monoid (AddNodeRes d) where
  mempty = AddNodeRes ChangedNo Seq.empty
  mappend = (<>)

-- private
egAddNodeSub :: (Show (f EClassId), EAnalysis d f q, Functor f, Eq (f EClassId), Hashable (f EClassId)) => q -> f (ENodeTriple d) -> State (EGraph d f) (Changed, ENodeTriple d)
egAddNodeSub q fcd = do
  let fc = fmap entClass fcd
  -- important: node should already be canonicalized!
  -- first lookup the node in the assoc to ensure uniqueness
  mayNodeId <- gets (assocLookupByValue fc . egNodeAssoc)
  case mayNodeId of
    Just n -> do
      x <- gets (ILM.partialLookup n . egHashCons)
      eci <- gets (ILM.partialLookup x . egClassMap)
      let d = eciData eci
      pure (ChangedNo, ENodeTriple n x d)
    Nothing -> state $ \eg ->
      -- node does not exist; get new node and class ids
      let (n, nodeSource') = sourceAddInc (egNodeSource eg)
          (x, classSource') = sourceAddInc (egClassSource eg)
          -- add it to the uf (can discard return value since it's a new id, will be the same)
          (_, ef') = efAddInc x (egEquivFind eg)
          -- insert into the hashcons
          hc' = ILM.insert n x (egHashCons eg)
          -- analyze the node and put that info in the class map
          d = eaMake q (fmap entData fcd)
          eci = EClassInfo d (ILS.singleton n) ILS.empty
          classMap' = ILM.insert x eci (egClassMap eg)
          eg' = eg { egNodeSource = nodeSource', egClassSource = classSource', egEquivFind = ef', egHashCons = hc', egClassMap = classMap' }
      in ((ChangedYes, ENodeTriple n x d), eg')

-- private
-- Similar in structure to foldWholeTrackM
egAddTermSub :: (Show (f EClassId), EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (AddNodeRes d, ENodeTriple d)
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
    (changed2, z@(ENodeTriple n _ _)) <- egAddNodeSub q fx
    -- now update all its children to add this as a parent
    unless (Seq.null children) $
      modify' $ \eg ->
        let cm' = foldl' (\cm (ENodeTriple _ c _) -> ILM.adjust (\v -> v { eciParents = ILS.insert n (eciParents v) }) c cm) (egClassMap eg) children
        in eg { egClassMap = cm' }
    pure (AddNodeRes (changed1 <> changed2) (Seq.singleton z), z)

-- | Adds a term (recursively) to the graph. If already in the graph, returns 'ChangedNo' and existing class id. Otherwise
-- returns 'ChangedYes' and a new class id.
egAddTerm :: (Show (f EClassId), EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (Changed, EClassId)
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
  -- traceM (unwords ["MERGE", "cs=", show cs])
  mayRoots <- fmap (\ef -> traverse (`efFindRoot` ef) (ILS.toList cs)) (gets egEquivFind)
  case mayRoots of
    Nothing -> pure Nothing
    Just roots ->
      let rootsSet = ILS.fromList roots
      in if ILS.size rootsSet < 2
        then pure (Just ChangedNo)
        else do
          modify' (\eg -> eg { egWorkList = egWorkList eg :|> rootsSet })
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
-- Folds over items in worklist to merge, returning:
-- 1. map of old class -> new class for changed classes only
-- 2. closure of remapped classes (includes roots)
egRebuildMerge :: WorkList -> State (EGraph d f) (IntLikeMap EClassId EClassId, IntLikeSet EClassId)
egRebuildMerge wl = finalRes where
  finalRes = state $ \eg ->
    let ef = egEquivFind eg
        dc = egDeadClasses eg
    in case efMergeSetsInc (toList wl) ef of
      EquivMergeSetsResChanged roots classRemapSet ef' ->
        let classRemap = ILM.fromList (fmap (\c -> (c, efLookupRoot c ef')) (ILS.toList classRemapSet))
            closure = ILS.difference (efClosure (ILS.toList roots) ef') dc
        in ((classRemap, closure), eg { egEquivFind = ef' })
      _ -> ((ILM.empty, ILS.empty), eg)

-- private
-- Loop through nodes of all changed classes and update the hashcons to point to new classes
egRebuildHashCons :: IntLikeMap EClassId EClassId -> State (EGraph d f) ()
egRebuildHashCons classRemap =
  modify' (\eg -> let hc' = foldl' (go (egClassMap eg)) (egHashCons eg) (ILM.toList classRemap) in eg { egHashCons = hc' }) where
  go cm hc (oldClassId, newClassId) =
    let eci = ILM.partialLookup oldClassId cm
        nodes = eciNodes eci
    in foldl' (flip (`ILM.insert` newClassId)) hc (ILS.toList nodes)

-- private
egRebuildAssoc :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => IntLikeMap ENodeId EClassId -> IntLikeMap EClassId EClassId -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeSet EClassId, IntLikeMap ENodeId ENodeId, WorkList)
egRebuildAssoc origHc classRemap touchedClasses = do
  hc <- gets egHashCons
  cm <- gets egClassMap
  -- For each class that we're going to merge
  stateFold (ILS.empty, ILM.empty, Empty) (ILS.toList touchedClasses) $ \(ps, m, parentWl) c -> do
    -- traceM (unwords ["REBUILD ASSOC CLASS", "c=", show c])
    -- Get the class info
    let eci = ILM.partialLookup c cm
    -- For each node in the class
    (finalM, finalChanged, finalParentWl) <- stateFold (m, False, parentWl) (ILS.toList (eciNodes eci)) $ \(m', changed', parentWl') n -> do
      -- Canonicalize it and add to the node map
      n' <- egCanonicalizeInternal n
      if n == n'
        then pure (m', changed', parentWl')
        else do
          let m'' = ILM.insert n n' m'
          -- See if by canonicalizing we've induced a new equivalence
          let oldNodeClass = ILM.partialLookup n hc
              newNodeClass = ILM.partialLookup n' hc
              parentWl'' = if oldNodeClass == newNodeClass then parentWl' else parentWl' :|> ILS.fromList [oldNodeClass, newNodeClass]
          pure (m'', True, parentWl'')
    -- Emit parents only class has changed or if any nodes have changed during canonicalization
    -- Note that we look up parents in the ORIGINAL hashcons because those are the ones that have the nodes pointing to this
    let emitParents = finalChanged || ILM.member c classRemap
        addlParents = ILS.map (`ILM.partialLookup` origHc) (eciParents eci)
        ps' = if emitParents then ILS.union addlParents ps else ps
    -- traceM (unwords ["REBUILD ASSOC CLASS END", "c=", show c, "emitParents=", show emitParents, "addlParents=", show addlParents])
    pure (ps', finalM, finalParentWl)

-- private
egRebuildCanonWl :: IntLikeMultiMap ENodeId ENodeId -> State (EGraph d f) WorkList
egRebuildCanonWl nodeMultiMap = goRoot where
  goRoot = do
    hc <- gets egHashCons
    -- For each node in the new -> old multimap
    pure (foldl' (goEach hc) Empty (ILMM.toList nodeMultiMap))
  goEach hc ms (_, oldNodes) =
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
egRebuildNodeRound :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => IntLikeMap ENodeId EClassId -> WorkList -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeSet EClassId, WorkList, IntLikeSet EClassId)
egRebuildNodeRound origHc wl parents = do
  -- traceM (unwords ["#########################\n", "START ROUND", "wl=", show wl, "parents=", show parents])
  -- First merge all classes together and get merged class sets
  (classRemap, classClosure) <- egRebuildMerge wl
  -- traceM (unwords ["POST MERGE", "classRemap=", show classRemap, "classClosure=", show classClosure])
  -- ef <- gets egEquivFind
  -- traceM (unwords ["POST EF", "ef=", show ef])
  -- Now update the hashcons so node ids point to merged classes
  egRebuildHashCons classRemap
  -- hc <- gets egHashCons
  -- traceM (unwords ["POST HASHCONS", "hc=", show hc])
  -- Track all classes touched here
  let touchedClasses = ILS.union parents classClosure
  -- Traverse all classes and canonicalize their nodes,
  -- recording the mapping from old -> new
  -- Also track all possible parent classes
  -- TODO can probably just rebuild node-by-node: go through self-parent nodes of newly merged classes plus usual parents
  (candParents, nodeMap, parentWl) <- egRebuildAssoc origHc classRemap touchedClasses
  -- traceM (unwords ["POST ASSOC", "candParents=", show candParents, "nodeMap=", show nodeMap, "parentWl=", show parentWl])
  -- Invert the node map to find new equivalences
  -- This produces a multimap of new -> set of old
  let nodeMultiMap = ILMM.fromInvertedMap nodeMap
  -- Find new equivalences induced by canonicalization
  canonWl <- egRebuildCanonWl nodeMultiMap
  -- Track next worklist
  let finalWl = parentWl <> canonWl
  -- Track parent classes for next round
  let finalParents = ILS.difference candParents touchedClasses
  -- traceM (unwords ["END ROUND", "nodeMap=", show nodeMap, "canonWl=", show canonWl, "finalParents=", show finalParents])
  pure (touchedClasses, finalWl, finalParents)

egRebuildClassSingle :: EAnalysis d f q => q -> IntLikeMap ENodeId EClassId -> IntLikeSet ENodeId -> EClassId -> IntLikeSet EClassId -> IntLikeMap EClassId (EClassInfo d) -> IntLikeMap EClassId (EClassInfo d)
egRebuildClassSingle q hc deadNodes newClass oldClasses initCm =
  let EClassInfo rootData rootNodes rootParents = ILM.partialLookup newClass initCm
      finalData = eaJoin q rootData (fmap (\c -> eciData (ILM.partialLookup c initCm)) (ILS.toList oldClasses))
      -- drop dead self nodes here
      allNodes = foldl' (\s c -> ILS.union s (eciNodes (ILM.partialLookup c initCm))) rootNodes (ILS.toList oldClasses)
      finalNodes = ILS.difference allNodes deadNodes
      -- drop self parents here because it's expensive otherwise. dead nodes will be compacted later
      nonSelfParents c = ILS.filter (\n -> ILM.partialLookup n hc /= c) (eciParents (ILM.partialLookup c initCm))
      finalParents = foldl' (\s c -> ILS.union s (nonSelfParents c)) rootParents (ILS.toList oldClasses)
      finalInfo = EClassInfo finalData finalNodes finalParents
      finalCm = ILM.insert newClass finalInfo initCm
  in finalCm

-- private
-- Rebuilds the classmap: merges old class infos into root class infos
-- Returns list of modified root classes
egRebuildClassMap :: EAnalysis d f q => q -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeMultiMap EClassId EClassId)
egRebuildClassMap q touchedClasses = state $ \eg ->
  let ef = egEquivFind eg
      dc = egDeadClasses eg
      hc = egHashCons eg
      deadNodes = ILS.fromList (assocDead (egNodeAssoc eg))
      roots = ILS.map (`efLookupRoot` ef) touchedClasses
      rootMap = ILM.fromList (fmap (\r -> (r, ILS.difference (efLookupLeaves r ef) dc)) (ILS.toList roots))
      cm' = foldl' (\cm (r, vs) -> egRebuildClassSingle q hc deadNodes r vs cm) (egClassMap eg) (ILM.toList rootMap)
      dc' = foldl' ILS.union (egDeadClasses eg) (ILM.elems rootMap)
  in (rootMap, eg { egClassMap = cm', egDeadClasses = dc' })

-- | Rebuilds the 'EGraph' after merging to allow adding more terms. (Always safe to call.)
egRebuild :: (EAnalysis d f q, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> State (EGraph d f) (IntLikeMultiMap EClassId EClassId)
egRebuild q = goRec where
  goRec = do
    -- Note the existing hashcons
    origHc <- gets egHashCons
    -- Read and clear the worklist - from now on nothing should add to it
    wl <- egTakeWorklist
    -- Merge and induce equivalences
    tc <- goNodeRounds origHc ILS.empty wl ILS.empty
    -- Now everything is merged so we only have to rewrite the changed parts of the classmap
    egRebuildClassMap q tc
  goNodeRounds !origHc !tc !wl !parents =
    if null wl && ILS.null parents
      then pure tc
      else do
        (newTc, newWl, newParents) <- egRebuildNodeRound origHc wl parents
        let mergedTc = ILS.union newTc tc
        goNodeRounds origHc mergedTc newWl newParents

egCanCompact :: EGraph d f -> Bool
egCanCompact eg = assocCanCompact (egNodeAssoc eg) || not (ILS.null (egDeadClasses eg))

-- TODO take node map and remap instead of removing
egCompactParentClass :: IntLikeSet ENodeId -> EClassInfo d -> EClassInfo d
egCompactParentClass deadNodes (EClassInfo dat nodes parents) =
  EClassInfo dat nodes (ILS.difference parents deadNodes)

findDeadNodeParentClasses :: Foldable f => IntLikeMap ENodeId (f EClassId) -> [ENodeId] -> IntLikeSet EClassId
findDeadNodeParentClasses fwd = foldl' go ILS.empty where
  go s n = foldl' (flip ILS.insert) s (ILM.partialLookup n fwd)

-- Requires that class be rebuilt!
egCompactInc :: Foldable f => EGraph d f -> EGraph d f
egCompactInc eg =
  let ef = egEquivFind eg
      assoc = egNodeAssoc eg
      hc = egHashCons eg
      cm = egClassMap eg
      deadClasses = egDeadClasses eg
      -- remove dead nodes from assoc
      (rootNodeMap, assoc') = assocCompactInc assoc
      deadNodes = ILS.unions (ILM.elems rootNodeMap)
      -- select all live classes containing dead nodes
      deadNodeParentClasses = findDeadNodeParentClasses (assocFwd assoc) (ILS.toList deadNodes)
      -- remove dead classes from hashcons
      hc' = foldl' (flip ILM.delete) hc (ILS.toList deadNodes)
      -- remove dead classes from unionfind
      (_, ef') = efCompactInc ef
      -- remove dead classes from classmap
      cm' = foldl' (flip ILM.delete) cm (ILS.toList deadClasses)
      -- remove dead nodes from classmap
      -- TODO remove set and pass map directly
      cm'' = foldl' (flip (ILM.adjust (egCompactParentClass deadNodes))) cm' (ILS.toList deadNodeParentClasses)
  in eg { egEquivFind = ef', egNodeAssoc = assoc', egClassMap = cm'', egHashCons = hc', egDeadClasses = ILS.empty }

egCompact :: Foldable f => State (EGraph d f) ()
egCompact = modify' egCompactInc
