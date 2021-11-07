{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An E-Graph implementation
module Overeasy.EGraph
  ( EClassId (..)
  , ENodeId (..)
  , EAnalysis (..)
  , EAnalysisOff (..)
  , ENodePair (..)
  , EClassInfo (..)
  , EGraph
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
import Control.Monad (foldM, unless, void)
import Control.Monad.State.Strict (State, get, gets, modify', put)
import Data.Foldable (for_)
import Data.Functor.Foldable (project)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocCanCompact, assocCompactInc, assocEnsure, assocLookupByValue, assocNew,
                       assocPartialLookupByKey, assocUpdate)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (stateLens)
import Overeasy.UnionFind (MergeRes (..), UnionFind, ufAdd, ufFind, ufMerge, ufNew, ufPartialFind, ufRoots, ufSize,
                           ufTotalSize)
-- import Debug.Trace (traceM)

-- | An opaque class id
newtype EClassId = EClassId { unEClassId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | An opaque node id
newtype ENodeId = ENodeId { unENodeId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

-- | The definition of an 'EGraph' analysis.
-- We thread the analysis definition 'q' through the 'EGM' monad to perform analyses as we construct
-- and manipulate the graph. Only its data 'd' is persisted in the graph itself.
-- Should obey:
--   The related data must obey 'BoundedJoinSemilattice'
--   'eaModify' is idempotent
class EAnalysis d f q | q -> d f where
  eaMake :: q -> f EClassId -> EGraph d f -> d
  eaJoin :: q -> d -> d -> d
  eaModify :: q -> EClassId -> EGraph d f -> EGraph d f

-- | A disabled analysis
data EAnalysisOff (f :: Type -> Type) = EAnalysisOff

instance EAnalysis () f (EAnalysisOff f) where
  eaMake _ _ _ = ()
  eaJoin _ _ _ = ()
  eaModify _ _ g = g

data ENodePair = ENodePair
  { enpNode :: !ENodeId
  , enpClass :: !EClassId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Info stored for every class: analysis data and class members.
data EClassInfo d = EClassInfo
  { eciData :: !d
  , eciNodes :: !(IntLikeSet ENodeId)
  , eciParents :: !(IntLikeMap ENodeId EClassId)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

eciJoin :: EAnalysis d f q => q -> EClassInfo d -> EClassInfo d -> EClassInfo d
eciJoin q (EClassInfo d1 n1 p1) (EClassInfo d2 n2 p2) = EClassInfo (eaJoin q d1 d2) (n1 <> n2) (p1 <> p2)

-- private ctor
data EGraph d f = EGraph
  { egSource :: !(Source EClassId)
  , egUnionFind :: !(UnionFind EClassId)
  , egClassMap :: !(IntLikeMap EClassId (EClassInfo d))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  -- TODO change name from egHashCons to egNodeMap
  , egHashCons :: !(IntLikeMap ENodeId EClassId)
  , egWorkList :: ![EClassId]
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
egNew = EGraph (sourceNew (EClassId 0)) ufNew ILM.empty (assocNew (ENodeId 0)) ILM.empty []

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

-- private
egMake :: EAnalysis d f q => q -> f EClassId -> State (EGraph d f) d
egMake q fc = fmap (eaMake q fc) get

-- private
egModify :: EAnalysis d f q => q -> EClassId -> State (EGraph d f) ()
egModify q x = modify' (eaModify q x)

data AddNodeRes = AddNodeRes !Changed !(Seq ENodePair)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup AddNodeRes where
  AddNodeRes c1 p1 <> AddNodeRes c2 p2 = AddNodeRes (c1 <> c2) (p1 <> p2)

instance Monoid AddNodeRes where
  mempty = AddNodeRes ChangedNo Seq.empty
  mappend = (<>)

-- private
egAddNodeSub :: (EAnalysis d f q, Eq (f EClassId), Hashable (f EClassId)) => q -> f EClassId -> State (EGraph d f) (AddNodeRes, ENodePair)
egAddNodeSub q fc = do
  -- important: node should already be canonicalized!
  -- first lookup the node in the assoc to ensure uniqueness
  (c, n) <- stateLens egNodeAssocL (assocEnsure fc)
  x <- case c of
        ChangedNo -> do
          -- node already exists; just return existing class id
          -- partial: should exist in hashcons by construction (next case)
          -- and should be mapped to the correct root class
          gets (ILM.partialLookup n . egHashCons)
        ChangedYes -> do
          -- node does not exist; get a new class id
          x <- stateLens egSourceL sourceAdd
          -- add it to the uf
          stateLens egUnionFindL (ufAdd x)
          -- map the node to the class id
          stateLens egHashConsL (modify' (ILM.insert n x))
          -- analyze the node and put that info in the class map
          d <- egMake q fc
          let i = EClassInfo d (ILS.singleton n) ILM.empty
          stateLens egClassMapL (modify' (ILM.insert x i))
          -- call analysis modify
          egModify q x
          pure x
  let p = ENodePair n x
  pure (AddNodeRes c (Seq.singleton p), p)

-- private
-- Similar in structure to foldWholeTrackM
egAddTermSub :: (EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (AddNodeRes, EClassId)
egAddTermSub q = go where
  go t = do
    -- unwrap to work with the functor layer
    let ft = project t
    -- add all child nodes
    frx <- traverse go ft
    -- collect info generated from child nodes and leave pure structure
    let (AddNodeRes changed1 children1, fx) = sequenceA frx
    -- now fx should be canonicalized by construction
    -- add the node to get its node and class ids
    (AddNodeRes changed2 children2, ENodePair n x) <- egAddNodeSub q fx
    -- now update all its children to add this as a parent
    for_ children1 $ \(ENodePair _ c) ->
      -- it's ok to overwrite class ids per node because they're guaranteed to be in the same equivalence class
      stateLens egClassMapL (modify' (ILM.adjust (\v -> v { eciParents = ILM.insert n x (eciParents v) }) c))
    pure (AddNodeRes (changed1 <> changed2) children2, x)

-- | Adds a term (recursively) to the graph. If already in the graph, returns 'ChangedNo' and existing class id. Otherwise
-- returns 'ChangedYes' and a new class id.
egAddTerm :: (EAnalysis d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> t -> State (EGraph d f) (Changed, EClassId)
egAddTerm q t = fmap (\(AddNodeRes c _, x) -> (c, x)) (egAddTermSub q t)

-- | Merges two classes:
-- Returns 'Nothing' if the classes are not found
-- Otherwise returns the merged class id and whether anything has changed
-- If things have changed, then you must call 'egRebuild' before adding more terms.
-- (You can use 'egNeedsRebuild' to query this.)
egMerge :: EAnalysis d f q => q -> EClassId -> EClassId -> State (EGraph d f) (Maybe (Changed, EClassId))
egMerge q i j = do
  -- traceM (show ["MERGE", show i, show j])
  -- merge classes in the uf
  mx <- stateLens egUnionFindL (ufMerge i j)
  case mx of
    MergeResMissing _ -> pure Nothing
    MergeResUnchanged x -> pure (Just (ChangedNo, x))
    MergeResChanged leftRoot rightRoot mergedRoot -> do
      -- lookup and merge data for previous roots
      -- partial: guaranteed present by add and merge
      leftInfo <- gets (fromJust . egClassInfo leftRoot)
      rightInfo <- gets (fromJust . egClassInfo rightRoot)
      let mergedInfo = eciJoin q leftInfo rightInfo
          classToDelete = if mergedRoot == leftRoot then rightRoot else leftRoot
      -- add the merged entry and remove the old entry from the class map
      stateLens egClassMapL (modify' (ILM.insert mergedRoot mergedInfo . ILM.delete classToDelete))
      -- We don't need to update the hashcons here because we can do it all at once in the rebuild
      stateLens egWorkListL (modify' (mergedRoot:))
      pure (Just (ChangedYes, mergedRoot))

-- | Have we merged classes and do we need to rebuild before adding more terms?
egNeedsRebuild :: EGraph d f -> Bool
egNeedsRebuild = not . null . egWorkList

-- private
-- Take the worklist (swapping for []) and deduplicate it.
-- Since the worklist is reversed, folding and consing again will emit
-- deduplicated work in the correct order. Note that it's not strictly necessary
-- to work in order but it makes it easy to follow along.
egTakeWorklist :: State (EGraph d f) [EClassId]
egTakeWorklist = go1 where
  go1 = do
    wl <- gets egWorkList
    case wl of
      [] -> pure []
      _ -> do
        stateLens egWorkListL (put [])
        (wlRoots, _) <- stateLens egUnionFindL (foldM (uncurry go2) ([], ILS.empty) wl)
        pure wlRoots
  go2 wlR wlS c = do
    d <- ufPartialFind c
    pure (ILS.insertState (\present -> if present then wlR else d:wlR) d wlS)

-- | Rebuilds the 'EGraph' after merging to allow adding more terms. (Always safe to call.)
egRebuild :: (EAnalysis d f q, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => q -> State (EGraph d f) ()
egRebuild q = goRec where
  goRec = do
    wl <- egTakeWorklist
    unless (null wl) $ do
      -- for each class, repair nodes to update canonical map
      for_ wl egRepairNodes
      -- for each class, repair parents to introduce new equalities
      acc <- foldM goRepair [] wl
      -- merge according to the new equalities and recurse
      for_ (reverse acc) (\(i, j) -> void (egMerge q i j))
      goRec
  goRepair acc i = do
    toMerge <- egRepairParents i
    pure (toMerge ++ acc)
    -- wl <- gets egWorkList
    -- unless (null wl) $ do
    --   -- take the worklist
    --   stateLens egWorkListL (put [])
    --   -- deduplicate the worklist for to merged classes
    --   -- TODO go through this in order of added (worklist is reversed, and here comes out by order)
    --   -- partial: find guaranteed by presence in worklist
    --   wlRoots <- stateLens egUnionFindL (foldM (\rs c -> fmap (`ILS.insert` rs) (ufPartialFind c)) ILS.empty wl)
    --   -- for each class, repair parents to introduce new equalities
    --   for_ (ILS.toList wlRoots) (egRepairParents q)
    --   -- for each class, repair nodes to update canonical map
    --   -- TODO need to lookup roots again?
    --   for_ (ILS.toList wlRoots) (egRepairNodes q)
    --   -- loop until the worklist is empty
    --   goRec

-- private
egRepairNodes :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => EClassId -> State (EGraph d f) ()
egRepairNodes i = go where
  go = do
    -- traceM (show ["REPAIR NODES", show i])
    nodeIds <- gets (eciNodes . ILM.partialLookup i . egClassMap)
    newNodeIds <- for (ILS.toList nodeIds) $ \nodeId -> do
      newNodeId <- egCanonicalizeInternal nodeId
      -- traceM (show ["REPAIR NODE OF CLASS", show i, show nodeId, show newNodeId])
      -- TODO keep, delete old node id?
      -- stateLens egHashConsL (modify' (ILM.insert newNodeId i . ILM.delete nodeId i))
      stateLens egHashConsL (modify' (ILM.insert newNodeId i))
      pure newNodeId
    let newNodes = ILS.fromList newNodeIds
    stateLens egClassMapL (modify' (ILM.adjust (\eci -> eci { eciNodes = newNodes }) i))

-- private
egRepairParents :: (Traversable f, Eq (f EClassId), Hashable (f EClassId)) => EClassId -> State (EGraph d f) [(EClassId, EClassId)]
egRepairParents i = go where
  go = do
    -- traceM (show ["REPAIR PARENTS", show i])
    -- partials: nodes and classes should be present by construction
    classInfo <- gets (ILM.partialLookup i . egClassMap)
    let selfNodes = eciNodes classInfo
        parentPairs = filter (\(n, _) -> not (ILS.member n selfNodes)) (ILM.toList (eciParents classInfo))
    -- update parents
    newParentPairs <- for parentPairs $ \(parNodeId, parClassId) -> do
      -- first update the assoc to canonicalize node
      newParNodeId <- egCanonicalizeInternal parNodeId
      newParClassId <- stateLens egUnionFindL (ufPartialFind parClassId)
      -- traceM (show ["REPAIR PARENT NODE", show i, show parNodeId, show parClassId, show newParNodeId, show newParClassId])
      -- update the hashcons to point canonical node to canonical class
      stateLens egHashConsL (modify' (ILM.insert newParNodeId newParClassId))
      pure (newParNodeId, newParClassId)
    -- dedupe the parents - equal parents get merged and put on worklist
    -- some parents may map canonically to the same node now, and so their classes need to be merged
    let (toMerge, finalParents) = foldr (dedupe selfNodes) ([], ILM.empty) newParentPairs
    stateLens egClassMapL (modify' (ILM.adjust (\eci -> eci { eciParents = finalParents }) i))
    -- traceM (show ["REPAIR PARENT TOMERGE", show toMerge])
    pure toMerge
  -- -- Fold over all (parent node, class) pairs to induce congruences and produce final parent map:
  -- -- 1. node not in self-class -> non-self-class: insert
  -- -- 2. node not in self-class -> self-class: merge that node's class with self-class
  -- -- 3. node in self-class -> _: ignore
  -- dedupe newParents pairs =
  --   case pairs of
  --     [] -> pure newParents
  --     (newParNodeId, newParClassId):rest -> do
  --       traceM (show ["REPAIR PARENT DEDUPE", show i, show newParNodeId, show newParClassId])
  --       -- need to find these each time because merges may have happened
  --       selfRootId <- stateLens egUnionFindL (ufPartialFind i)
  --       parRootId <- stateLens egUnionFindL (ufPartialFind newParClassId)
  --       if parRootId == selfRootId
  --         then
  --           -- case 3
  --           dedupe newParents rest
  --         else do
  --           selfNodes <- gets (eciNodes . ILM.partialLookup selfRootId . egClassMap)
  --           if ILS.member newParNodeId selfNodes
  --             then do
  --               -- case 2
  --               void (egMerge q selfRootId parRootId)
  --               dedupe newParents rest
  --             else do
  --               -- case 1
  --               let newParents' = ILM.insert newParNodeId parRootId newParents
  --               dedupe newParents' rest
  dedupe selfNodes (newParNodeId, newParClassId) (acc, newParents) =
    if ILS.member newParNodeId selfNodes
      then ((i, newParClassId):acc, newParents)
      else ILM.insertState (accUpdate newParClassId acc) newParNodeId newParClassId newParents
  accUpdate newParClassId acc mayClassId =
    case mayClassId of
      Just classId ->
        if classId == newParClassId
          then acc
          else (classId, newParClassId):acc
      Nothing -> acc

    -- traceM (show ["REPAIR PARENT DEDUPE", show i, show newParNodeId, show newParClassId])
    --     case ILM.lookup newParNodeId newParents of
    --       Just otherClassId -> void (egMerge q newParClassId otherClassId)
    --       Nothing -> pure ()
    --     let newParents' = ILM.insert newParNodeId newParClassId newParents
    --     dedupe newParents' rest
    -- (acc, newParents)

  -- dedupe selfNodes acc newParents pairs =
  --   case pairs of
  --     [] -> pure (acc, newParents)
  --     (newParNodeId, newParClassId):rest -> do
  --       traceM (show ["REPAIR PARENT DEDUPE", show i, show newParNodeId, show newParClassId])
  --       case ILM.lookup newParNodeId newParents of
  --         Just otherClassId -> void (egMerge q newParClassId otherClassId)
  --         Nothing -> pure ()
  --       let newParents' = ILM.insert newParNodeId newParClassId newParents
  --       dedupe newParents' rest

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
