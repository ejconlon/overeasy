{-# LANGUAGE DeriveAnyClass #-}
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
  , ClassReplacements
  , MergeResult (..)
  , egClassSource
  , egNodeSource
  , egEquivFind
  , egClassMap
  , egNodeAssoc
  , egHashCons
  , egClassSize
  , egNodeSize
  , egFindNode
  , egFindTerm
  , egClassInfo
  , egNew
  , egClasses
  , egCanonicalize
  , egCanonicalizePartial
  , egAddTerm
  , egMerge
  , egMergeMany
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.State.Strict (State, gets, modify', state)
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable (project)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import IntLike.Map (IntLikeMap (..))
import qualified IntLike.Map as ILM
import IntLike.MultiMap (IntLikeMultiMap)
import qualified IntLike.MultiMap as ILMM
import IntLike.Set (IntLikeSet (..))
import qualified IntLike.Set as ILS
import Overeasy.Assoc (Assoc, AssocInsertRes (..), assocCompactInc, assocInsertInc, assocLookupByValue, assocNew,
                       assocPartialLookupByKey)
import Overeasy.Classes (Changed (..))
import Overeasy.EquivFind (EquivFind (..), EquivMergeSetsRes (..), efAddInc, efCanonicalize, efCanonicalizePartial,
                           efClosure, efCompactInc, efFindRoot, efLookupRoot, efMergeSetsInc, efNew, efRoots,
                           efRootsSize, efSubset)
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAddInc, sourceNew)
import Overeasy.StateUtil (stateFold)
import qualified Data.IntMap.Strict as IntMap

-- | An opaque class id
newtype EClassId = EClassId { unEClassId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | An opaque node id
newtype ENodeId = ENodeId { unENodeId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | The definition of an 'EGraph' analysis.
-- d should be a join semilattice
-- eaMake should be convex
class (Semigroup d, Ord d) => EAnalysis d f q | q -> d f where
  eaMake :: q -> f d -> d

-- | A disabled analysis
data EAnalysisOff (f :: Type -> Type) = EAnalysisOff

instance EAnalysis () f (EAnalysisOff f) where
  eaMake _ _ = ()

newtype EAnalysisAlgebra d f = EAnalysisAlgebra
  { unEAnalysisAlgebra :: f d -> d
  }

instance (Semigroup d, Ord d) => EAnalysis d f (EAnalysisAlgebra d f) where
  eaMake = unEAnalysisAlgebra

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

type ClassReplacements = EquivFind EClassId

data MergeResult a =
    MergeResultUnchanged
  -- ^ All classes already merged, no change
  | MergeResultMissing !WorkItem
  -- ^ Some classes missing, returns first problematic worklist entry
  -- (note that not all classes in worklist item will be missing,
  -- only at least one from the set)
  | MergeResultChanged !a
  -- ^ Some classes merged, returns root map or merged class id
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- private ctor
data EGraph d f = EGraph
  { egClassSource :: !(Source EClassId)
  , egNodeSource :: !(Source ENodeId)
  , egEquivFind :: !(EquivFind EClassId)
  , egClassMap :: !(IntLikeMap EClassId (EClassInfo d))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(IntLikeMap ENodeId EClassId)
  } deriving stock (Generic)

deriving stock instance (Eq d, Eq (f EClassId)) => Eq (EGraph d f)
deriving stock instance (Show d, Show (f EClassId)) => Show (EGraph d f)
deriving anyclass instance (NFData d, NFData (f EClassId)) => NFData (EGraph d f)

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
egNew = EGraph (sourceNew (EClassId 0)) (sourceNew (ENodeId 0)) efNew ILM.empty assocNew ILM.empty

-- | Yields all root classes
egClasses :: State (EGraph d f) [EClassId]
egClasses = gets (efRoots . egEquivFind)

-- | Find the canonical form of a node.
-- If any classes are missing, the first missing is returned.
egCanonicalize :: Traversable f => f EClassId -> State (EGraph d f) (Either EClassId (f EClassId))
egCanonicalize fc = gets (efCanonicalize fc . egEquivFind)

-- | Find the canonical form of a node.
-- If any classes are missing, simply skip them.
egCanonicalizePartial :: Traversable f => f EClassId -> State (EGraph d f) (f EClassId)
egCanonicalizePartial fc = gets (efCanonicalizePartial fc . egEquivFind)

-- private
egCanonicalizeInternal :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => ENodeId -> State (EGraph d f) (ENodeId, Maybe (IntLikeSet ENodeId))
egCanonicalizeInternal x = state $ \eg ->
  let ef = egEquivFind eg
      assoc = egNodeAssoc eg
      node = assocPartialLookupByKey x assoc
      -- TODO analyze here
      fz = efCanonicalizePartial node ef
      ((y, res), assoc') = assocInsertInc x fz assoc
  in case res of
    AssocInsertResUnchanged -> ((y, Nothing), eg)
    AssocInsertResMerged toDelete ->
      ((y, Just toDelete), eg { egNodeAssoc = assoc' })
    _ -> ((y, Nothing), eg { egNodeAssoc = assoc' })

-- private
data AddNodeRes d = AddNodeRes !Changed !(Seq (ENodeTriple d))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup (AddNodeRes d) where
  AddNodeRes c1 p1 <> AddNodeRes c2 p2 = AddNodeRes (c1 <> c2) (p1 <> p2)

instance Monoid (AddNodeRes d) where
  mempty = AddNodeRes ChangedNo Seq.empty
  mappend = (<>)

-- private
egAddNodeSub :: (EAnalysis d f q, Functor f, Eq (f EClassId), Hashable (f EClassId)) =>q -> f (ENodeTriple d) -> State (EGraph d f) (Changed, ENodeTriple d)
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
          -- add it to the assoc (ignore and partial by construction)
          (_, assoc') = assocInsertInc n fc (egNodeAssoc eg)
          -- insert into the hashcons
          hc' = ILM.insert n x (egHashCons eg)
          -- analyze the node and put that info in the class map
          d = eaMake q (fmap entData fcd)
          eci = EClassInfo d (ILS.singleton n) ILS.empty
          classMap' = ILM.insert x eci (egClassMap eg)
          eg' = eg { egNodeSource = nodeSource', egClassSource = classSource', egEquivFind = ef', egNodeAssoc = assoc', egHashCons = hc', egClassMap = classMap' }
      in ((ChangedYes, ENodeTriple n x d), eg')

-- private
-- Similar in structure to foldWholeTrackM
egAddTermSub :: (EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) =>q -> t -> State (EGraph d f) (AddNodeRes d, ENodeTriple d)
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
        -- Add node to class parents (unless it's a self parent)
        let cm' = foldl' (\cm (ENodeTriple _ c _) -> ILM.adjust (\v -> if ILS.member n (eciNodes v) then v else v { eciParents = ILS.insert n (eciParents v) }) c cm) (egClassMap eg) children
        in eg { egClassMap = cm' }
    pure (AddNodeRes (changed1 <> changed2) (Seq.singleton z), z)

-- | Adds a term (recursively) to the graph. If already in the graph, returns 'ChangedNo' and existing class id. Otherwise
-- returns 'ChangedYes' and a new class id.
egAddTerm :: (EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) =>q -> t -> State (EGraph d f) (Changed, EClassId)
egAddTerm q t = fmap (\(AddNodeRes c _, ENodeTriple _ x _) -> (c, x)) (egAddTermSub q t)

-- | Merges two classes:
-- Returns 'Nothing' if the classes are not found or if they're already equal.
-- Otherwise returns the class remapping.
-- Note that it's MUCH more efficient to accumulate a 'WorkList' and use 'egMergeMany'.
egMerge :: (Semigroup d, Traversable f, Eq (f EClassId), Hashable (f EClassId))
  => EClassId -> EClassId -> State (EGraph d f) (MergeResult EClassId)
egMerge i j = do
  mr <- egMergeMany (Seq.singleton (ILS.fromList [i, j]))
  -- We're guaranteed to have one and only one root in the map, so this won't fail
  pure (fmap (fst . head . ILM.toList . efFwd) mr)

data BuildWorkResult a =
    BuildWorkResultUnchanged
  | BuildWorkResultMissing !WorkItem
  | BuildWorkResultOk !a

-- private
egBuildWorkItem :: WorkItem -> State (EGraph d f) (BuildWorkResult WorkItem)
egBuildWorkItem cs = do
  mayRoots <- fmap (\ef -> traverse (`efFindRoot` ef) (ILS.toList cs)) (gets egEquivFind)
  pure $! case mayRoots of
    Nothing -> BuildWorkResultMissing cs
    Just roots ->
      let rootsSet = ILS.fromList roots
      in if ILS.size rootsSet < 2
        then BuildWorkResultUnchanged
        else BuildWorkResultOk rootsSet

-- private
egBuildWorklist :: WorkList -> State (EGraph d f) (BuildWorkResult WorkList)
egBuildWorklist = go Empty where
  go !acc = \case
    Empty ->
      pure $! if Seq.null acc
        then BuildWorkResultUnchanged
        else BuildWorkResultOk acc
    cs :<| wl' -> do
      rcs <- egBuildWorkItem cs
      case rcs of
        BuildWorkResultUnchanged -> go acc wl'
        BuildWorkResultMissing cs' -> pure (BuildWorkResultMissing cs')
        BuildWorkResultOk cs' -> go (acc :|> cs') wl'

-- | Merges many sets of classes.
-- Returns 'Nothing' if the classes are not found or if they're already equal.
-- Otherwise returns the class remapping (equiv map of root to set of leaf classes).
-- It is important to note that the leaf classes in the returned mapping have been
-- REMOVED from the egraph, so they cannot be used to lookup classes in the future.
-- Therefore, if you have any eclasses stored externally, you'll want to (partially)
-- canonicalize with the returned mapping.
-- Also note that the analysis of a given class is going to be an UNDER-APPROXIMATION
-- of the true analysis value, because per-node analyses are not recomputed.
egMergeMany :: (Semigroup d, Traversable f, Eq (f EClassId), Hashable (f EClassId))
  => WorkList -> State (EGraph d f) (MergeResult ClassReplacements)
egMergeMany wl0 = do
  br <- egBuildWorklist wl0
  case br of
    BuildWorkResultUnchanged -> pure MergeResultUnchanged
    BuildWorkResultMissing cs -> pure (MergeResultMissing cs)
    BuildWorkResultOk wl1 -> fmap MergeResultChanged (egRebuild wl1)

-- private
-- Folds over items in worklist to merge, returning:
-- 1. map of old class -> new class for changed classes only
-- 2. closure of remapped classes (includes roots)
egRebuildMerge :: WorkList -> State (EGraph d f) (IntLikeMap EClassId EClassId, IntLikeSet EClassId)
egRebuildMerge wl = finalRes where
  finalRes = state $ \eg ->
    let ef = egEquivFind eg
    in case efMergeSetsInc (toList wl) ef of
      EquivMergeSetsResChanged roots classRemapSet ef' ->
        let classRemap = ILM.fromList (fmap (\c -> (c, efLookupRoot c ef')) (ILS.toList classRemapSet))
            closure = efClosure (ILS.toList roots) ef'
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
egRebuildAssoc :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => IntLikeMap ENodeId EClassId -> IntLikeMap EClassId EClassId -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeSet EClassId, WorkList)
egRebuildAssoc origHc classRemap touchedClasses = do
  hc <- gets egHashCons
  cm <- gets egClassMap
  -- For each class that we're going to merge
  stateFold (ILS.empty, Empty) (ILS.toList touchedClasses) $ \(ps, parentWl) c -> do
    -- Get the class info
    let eci = ILM.partialLookup c cm
    -- For each node in the class
    (finalChanged, finalParentWl) <- stateFold (False, parentWl) (ILS.toList (eciNodes eci)) $ \(changed', parentWl') n -> do
      -- Canonicalize it and add to the node map
      (newN, mayEquivNs) <- egCanonicalizeInternal n
      case mayEquivNs of
        Nothing -> pure (changed', parentWl')
        Just equivNs ->
          let allNs = ILS.insert newN equivNs
              allEquivClasses = ILS.map (`ILM.partialLookup` hc) allNs
          in if ILS.size allEquivClasses > 1
            then pure (True, parentWl' :|> allEquivClasses)
            else pure (changed', parentWl')
    -- Emit parents if:
    --   1. class has changed
    --   2. any nodes have changed during canonicalization
    --   3. TODO class analysis has changed
    -- Note that we look up parents in the ORIGINAL hashcons because those are the ones that have the nodes pointing to this
    let emitParents = finalChanged || ILM.member c classRemap
        addlParents = ILS.map (`ILM.partialLookup` origHc) (eciParents eci)
        ps' = if emitParents then ILS.union addlParents ps else ps
    pure (ps', finalParentWl)

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
-- One round of rebuilding
egRebuildNodeRound :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => IntLikeMap ENodeId EClassId -> WorkList -> IntLikeSet EClassId -> State (EGraph d f) (IntLikeSet EClassId, WorkList, IntLikeSet EClassId)
egRebuildNodeRound origHc wl parents = do
  -- First merge all classes together and get merged class sets
  (classRemap, classClosure) <- egRebuildMerge wl
  -- Now update the hashcons so node ids point to merged classes
  egRebuildHashCons classRemap
  -- Track all classes touched here
  let touchedClasses = ILS.union parents classClosure
  -- Traverse all classes and canonicalize their nodes,
  -- recording the mapping from old -> new
  -- Also track all possible parent classes
  -- TODO possible to rebuild node-by-node?
  (candParents, parentWl) <- egRebuildAssoc origHc classRemap touchedClasses
  -- Track parent classes for next round
  let finalParents = ILS.difference candParents touchedClasses
  pure (touchedClasses, parentWl, finalParents)

-- private
-- Rebuild just the class info corresponding to 'newClass'
egRebuildClassSingle :: Semigroup d => EClassId -> IntLikeSet EClassId -> IntLikeMap EClassId (EClassInfo d) -> IntLikeMap EClassId (EClassInfo d)
egRebuildClassSingle newClass oldClasses initCm =
  let EClassInfo rootData rootNodes rootParents = ILM.partialLookup newClass initCm
      finalData = sconcat (rootData :| fmap (\c -> eciData (ILM.partialLookup c initCm)) (ILS.toList oldClasses))
      -- keep dead self nodes here. will be dropped in compact
      finalNodes = foldl' (\s c -> ILS.union s (eciNodes (ILM.partialLookup c initCm))) rootNodes (ILS.toList oldClasses)
      -- keep dead parent nodes here, just exclude self nodes. will be dropped in compact
      lookupParents c = eciParents (ILM.partialLookup c initCm)
      candParents = foldl' (\s c -> ILS.union s (lookupParents c)) rootParents (ILS.toList oldClasses)
      finalParents = ILS.difference candParents finalNodes
      finalInfo = EClassInfo finalData finalNodes finalParents
      finalCm = ILM.insert newClass finalInfo initCm
  in finalCm

-- private
-- Rebuilds the classmap: merges old class infos into root class infos
-- Returns list of modified root classes
egRebuildClassMap :: Semigroup d => IntLikeSet EClassId -> State (EGraph d f) ClassReplacements
egRebuildClassMap touchedClasses = state $ \eg ->
  let ef = egEquivFind eg
      -- Find roots corresponding to all touched classes
      roots = ILS.map (`efLookupRoot` ef) touchedClasses
      -- Prepare a replacement map for external consumers that just contains changed classes
      classReplacements = efSubset (ILS.toList roots) ef
      -- Rebuild the class map (TODO is the difference necessary?)
      cm' = foldl' (\cm (r, vs) -> egRebuildClassSingle r vs cm) (egClassMap eg) (ILM.toList (efFwd classReplacements))
  in (classReplacements, eg { egClassMap = cm' })

-- private
-- Rebuilds the 'EGraph' - merges classes as requested in the worklist, recanonicalizes, and reanalyzes.
-- This may take several rounds as changes propagate "upward" to parents.
egRebuild :: (Semigroup d, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => WorkList -> State (EGraph d f) ClassReplacements
egRebuild wl0 = goRec where
  goRec = do
    -- Note the existing hashcons
    origHc <- gets egHashCons
    -- Merge and induce equivalences
    -- We track "touched classes" to know which to later rebuild in the classmap
    tc <- goNodeRounds origHc ILS.empty wl0 ILS.empty
    -- Now everything is merged, so rewrite the changed parts of the classmap
    rm <- egRebuildClassMap tc
    -- Finally, cleanup all "dead" classes and nodes
    egCompact rm
    -- And return the final class remapping
    pure rm
  goNodeRounds !origHc !tc !wl !parents =
    if null wl && ILS.null parents
      then pure tc
      else do
        (newTc, newWl, newParents) <- egRebuildNodeRound origHc wl parents
        let mergedTc = ILS.union newTc tc
        goNodeRounds origHc mergedTc newWl newParents

-- private
-- Replace parent nodes with correct (remapped) ones
egCompactParentClass :: IntLikeMap ENodeId ENodeId -> EClassInfo d -> EClassInfo d
egCompactParentClass nodeReplacements (EClassInfo dat nodes parents) =
  EClassInfo dat nodes (ILS.map (\n -> ILM.findWithDefault n n nodeReplacements) parents)

-- private
-- Remove dead nodes from given class info
egCompactSelfClass :: IntLikeMap ENodeId ENodeId -> EClassInfo d -> EClassInfo d
egCompactSelfClass nodeReplacements (EClassInfo dat nodes parents) =
  EClassInfo dat (ILS.filter (not . (`ILM.member` nodeReplacements)) nodes) parents

-- private
-- Find all classes that have dead nodes
findDeadNodeParentClasses :: Foldable f => Assoc ENodeId (f EClassId) -> [ENodeId] -> IntLikeSet EClassId
findDeadNodeParentClasses assoc = foldl' go ILS.empty where
  go s n = foldl' (flip ILS.insert) s (assocPartialLookupByKey n assoc)

-- private
-- Remove all dead nodes and classes from the graph
egCompactInc :: Foldable f => ClassReplacements -> EGraph d f -> EGraph d f
egCompactInc rm eg =
  let ef = egEquivFind eg
      assoc = egNodeAssoc eg
      hc = egHashCons eg
      cm = egClassMap eg
      deadClasses = IntLikeSet (IntMap.keysSet (unIntLikeMap (efBwd rm)))
      -- remove dead nodes from assoc
      (nodeReplacements, assoc') = assocCompactInc assoc
      -- select all live classes that are parents of dead nodes
      deadNodeParentClasses = findDeadNodeParentClasses assoc (ILM.keys nodeReplacements)
      -- select all live classes that contain dead nodes
      deadNodeSelfClasses = ILS.fromList (fmap (`ILM.partialLookup` hc) (ILM.keys nodeReplacements))
      -- remove dead classes from hashcons
      hc' = foldl' (flip ILM.delete) hc (ILM.keys nodeReplacements)
      -- remove dead classes from unionfind
      (_, ef') = efCompactInc ef
      -- remove dead classes from classmap
      cm' = foldl' (flip ILM.delete) cm (ILS.toList deadClasses)
      -- rewrite dead parent nodes in classmap
      cm'' = foldl' (flip (ILM.adjust (egCompactParentClass nodeReplacements))) cm' (ILS.toList deadNodeParentClasses)
      -- rewrite dead self nodes in classmap
      cm''' = foldl' (flip (ILM.adjust (egCompactSelfClass nodeReplacements))) cm'' (ILS.toList deadNodeSelfClasses)
  in eg { egEquivFind = ef', egNodeAssoc = assoc', egClassMap = cm''', egHashCons = hc' }

egCompact :: Foldable f => ClassReplacements -> State (EGraph d f) ()
egCompact = modify' . egCompactInc
