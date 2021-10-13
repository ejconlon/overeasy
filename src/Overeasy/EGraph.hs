{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An E-Graph implementation
module Overeasy.EGraph
  ( EClassId
  , ENodeId
  , EAnalysis (..)
  , EAnalysisOff (..)
  , ENodePair (..)
  , EClassInfo (..)
  , EGraph
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
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, get, gets, modify')
import Data.Foldable (for_)
import Data.Functor.Foldable (project)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocEnsure, assocFwd, assocNew)
import Overeasy.Classes (BoundedJoinSemilattice, Changed (..))
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (stateLens)
import Overeasy.UnionFind (MergeRes (..), UnionFind, ufAdd, ufFind, ufMerge, ufNew, ufRoots, ufSize, ufTotalSize)

-- | An opaque class id
newtype EClassId = EClassId { unEClassId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

-- | An opaque node id
newtype ENodeId = ENodeId { unENodeId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

-- | The definition of an 'EGraph' analysis.
-- We thread the analysis definition 'q' through the 'EGM' monad to perform analyses as we construct
-- and manipulate the graph. Only its data 'd' is persisted in the graph itself.
-- Should obey:
--   The related data must obey 'BoundedJoinSemilattice'
--   'eaModify' is idempotent
class BoundedJoinSemilattice d => EAnalysis d f q | q -> d f where
  eaMake :: q -> f EClassId -> EGraph d f -> d
  eaModify :: q -> EClassId -> EGraph d f -> EGraph d f

-- | A disabled analysis
data EAnalysisOff (f :: Type -> Type) = EAnalysisOff

instance EAnalysis () f (EAnalysisOff f) where
  eaMake _ _ _ = ()
  eaModify _ _ g = g

data ENodePair = ENodePair
  { enpNode :: !ENodeId
  , enpClass :: !EClassId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Info stored for every class: analysis data and class members.
data EClassInfo d = EClassInfo
  { eciData :: !d
  , eciNodes :: !(HashSet ENodeId)
  , eciParents :: !(HashMap ENodeId (HashSet EClassId))
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance BoundedJoinSemilattice d => Semigroup (EClassInfo d) where
  EClassInfo d1 n1 p1 <> EClassInfo d2 n2 p2 = EClassInfo (d1 <> d2) (n1 <> n2) (p1 <> p2)

-- private ctor
data EGraph d f = EGraph
  { egSource :: !(Source EClassId)
  , egUnionFind :: !(UnionFind EClassId)
  , egClassMap :: !(HashMap EClassId (EClassInfo d))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(HashMap ENodeId EClassId)
  , egWorkList :: !(HashSet EClassId)
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
egNodeSize = HashMap.size . egHashCons

-- | Lookup info for the given 'EClass'
egClassInfo :: EClassId -> EGraph d f -> Maybe (EClassInfo d)
egClassInfo c = HashMap.lookup c . egClassMap

-- | Find the class of the given node, if it exists.
-- Note that you may have to canonicalize first to find it!
egFindNode :: (Eq (f EClassId), Hashable (f EClassId)) => f EClassId -> EGraph d f -> Maybe EClassId
egFindNode fc eg = do
  n <- HashMap.lookup fc (assocFwd (egNodeAssoc eg))
  HashMap.lookup n (egHashCons eg)

-- | Find the class of the given term, if it exists
egFindTerm :: (RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => t -> EGraph d f -> Maybe EClassId
egFindTerm t eg = foldWholeM (`egFindNode` eg) t

-- | Creates a new 'EGraph'
egNew :: EGraph d f
egNew = EGraph (sourceNew (EClassId 0)) ufNew HashMap.empty (assocNew (ENodeId 0)) HashMap.empty HashSet.empty

-- | Yields all root classes
egClasses :: State (EGraph d f) (HashSet EClassId)
egClasses = stateLens egUnionFindL ufRoots

-- | Find the canonical form of a node
egCanonicalize :: Traversable f => f EClassId -> State (EGraph d f) (Maybe (f EClassId))
egCanonicalize = stateLens egUnionFindL . fmap sequence . traverse ufFind

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
          hc <- gets egHashCons
          -- partial: should exist in hashcons by construction (next case)
          pure (hc HashMap.! n)
        ChangedYes -> do
          -- node does not exist; get a new class id
          x <- stateLens egSourceL sourceAdd
          -- add it to the uf
          stateLens egUnionFindL (ufAdd x)
          -- map the node to the class id
          stateLens egHashConsL (modify' (HashMap.insert n x))
          -- analyze the node and put that info in the class map
          d <- egMake q fc
          let i = EClassInfo d (HashSet.singleton n) HashMap.empty
          stateLens egClassMapL (modify' (HashMap.insert x i))
          -- call analysis modify
          egModify q x
          pure x
  let p = ENodePair n x
  pure (AddNodeRes c (Seq.singleton p), p)

hmmInsert :: (Eq k, Hashable k, Eq v, Hashable v) => k -> v -> HashMap k (HashSet v) -> HashMap k (HashSet v)
hmmInsert k v = HashMap.insertWith (<>) k (HashSet.singleton v)

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
      stateLens egClassMapL (modify' (HashMap.adjust (\v -> v { eciParents = hmmInsert n x (eciParents v) }) c))
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
egMerge _ i j = do
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
      let mergedInfo = leftInfo <> rightInfo
      stateLens egClassMapL (modify' (HashMap.insert mergedRoot mergedInfo))
      -- add merged root to worklist
      stateLens egWorkListL (modify' (HashSet.insert mergedRoot))
      pure (Just (ChangedYes, mergedRoot))

-- | Have we merged classes and do we need to rebuild before adding more terms?
egNeedsRebuild :: EGraph d f -> Bool
egNeedsRebuild = not . HashSet.null . egWorkList

-- | Rebuilds the 'EGraph' after merging to allow adding more terms. (Always safe to call.)
egRebuild :: {- EAnalysis d f q => -} q -> State (EGraph d f) ()
egRebuild = error "TODO"
-- TODO implement rebuild + repair from the paper (fig 9)
