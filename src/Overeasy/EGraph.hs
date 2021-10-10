{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Overeasy.EGraph
  ( EClassId
  , ENodeId
  , EAnalysis (..)
  , EAGraph
  , EAnalysisOff (..)
  , EGC
  , EGM
  , runEGM
  , EGraph
  , egNew
  , egAddTerm
  , egMerge
  , egNeedsRebuild
  , egRebuild
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, gets, modify')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocEnsure, assocNew)
import Overeasy.Recursion (RecursiveWhole, foldWholeTrackM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (Changed (..), RSM, runRSM, stateLens)
import Overeasy.UnionFind (UnionFind, ufFind, ufMerge, ufNew)

newtype EClassId = EClassId { unEClassId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

newtype ENodeId = ENodeId { unENodeId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

class EAnalysis q where
  type EAData q :: Type
  type EAFunctor q :: Type -> Type
  eaMake :: q -> EAFunctor q EClassId -> EAGraph q -> EAData q
  eaMerge :: q -> EAData q -> EAData q -> (Changed, EAData q)
  eaModify :: q -> EClassId -> EAGraph q -> EAGraph q

type EAGraph q = EGraph (EAData q) (EAFunctor q)

data EAnalysisOff (f :: Type -> Type) = EAnalysisOff

instance EAnalysis (EAnalysisOff f) where
  type EAData (EAnalysisOff f) = ()
  type EAFunctor (EAnalysisOff f) = f
  eaMake _ _ _ = ()
  eaMerge _ _ _ = (ChangedNo, ())
  eaModify _ _ g = g

type EGC d f q = (d ~ EAData q, f ~ EAFunctor q, EAnalysis q)

type EGM d f q = RSM q (EGraph d f)

runEGM :: EGM d f q a -> q -> EGraph d f -> (a, EGraph d f)
runEGM = runRSM

data EClassInfo d = EClassInfo
  { eciData :: !d
  , eciNodes :: !(HashSet ENodeId)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

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

egNew :: EGraph d f
egNew = EGraph (sourceNew (EClassId 0)) ufNew HashMap.empty (assocNew (ENodeId 0)) HashMap.empty HashSet.empty

-- private
egCanonicalize :: Traversable f => f EClassId -> EGM d f q (Maybe (f EClassId))
egCanonicalize = stateLens egUnionFindL . fmap sequence . traverse ufFind

-- private
egMake :: EGC d f q => f EClassId -> EGM d f q d
egMake fc = do
  q <- ask
  fmap (eaMake q fc) get

-- private
egAddNode :: (EGC d f q, Eq (f EClassId), Hashable (f EClassId)) => f EClassId -> EGM d f q (Changed, EClassId)
egAddNode fc = do
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
          -- map the node to the class id
          stateLens egHashConsL (modify' (HashMap.insert n x))
          -- analyze the node and put that info in the class map
          d <- egMake fc
          let i = EClassInfo d (HashSet.singleton n)
          stateLens egClassMapL (modify' (HashMap.insert x i))
          pure x
  pure (c, x)

egAddTerm :: (EGC d f q, RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => t -> EGM d f q (Changed, EClassId)
egAddTerm = foldWholeTrackM egAddNode

egMerge :: EClassId -> EClassId -> EGM d f q (Maybe (Changed, EClassId))
egMerge i j = do
  mx <- stateLens egUnionFindL (ufMerge i j)
  case mx of
    Just (ChangedYes, x) -> stateLens egWorkListL (modify' (HashSet.insert x))
    _ -> pure ()
  pure mx

egNeedsRebuild :: EGraph d f -> Bool
egNeedsRebuild = not . HashSet.null . egWorkList

-- TODO implement rebuild + repair from the paper (fig 9)
egRebuild :: EGM d f q ()
egRebuild = error "TODO"
