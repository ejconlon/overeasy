{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Overeasy.EGraph
  ( EClassId
  , ENodeId
  , EGraphModify
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
import Control.Monad.State.Strict (gets, modify')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, assocEnsure, assocNew)
import Overeasy.Classes (ApplyAction, BoundedJoinSemilattice)
import Overeasy.Recursion (RecursiveWhole, foldWholeTrackM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (Changed (..), RSM, runRSM, stateLens)
import Overeasy.UnionFind (UnionFind, ufFind, ufMerge, ufNew)

newtype EClassId = EClassId { unEClassId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

newtype ENodeId = ENodeId { unENodeId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

type EGraphModify d f p = EClassId -> EGraph d f -> p

type EGC d f p = (BoundedJoinSemilattice d, ApplyAction d (EGraph d f))

type EGM d f p = RSM (EGraphModify d f p) (EGraph d f)

runEGM :: EGM d f p a -> EGraphModify d f p -> EGraph d f -> (a, EGraph d f)
runEGM = runRSM

-- private ctor
data EGraph d f = EGraph
  { egSource :: !(Source EClassId)
  , egUnionFind :: !(UnionFind EClassId)
  , egClassMap :: !(HashMap EClassId (HashSet ENodeId))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(HashMap ENodeId EClassId)
  , egWorkList :: !(HashSet EClassId)
  } deriving stock (Generic)

deriving stock instance Eq (f EClassId) => Eq (EGraph d f)
deriving stock instance Show (f EClassId) => Show (EGraph d f)
deriving anyclass instance NFData (f EClassId) => NFData (EGraph d f)

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
egCanonicalize :: Traversable f => f EClassId -> EGM d f p (Maybe (f EClassId))
egCanonicalize = stateLens egUnionFindL . fmap sequence . traverse ufFind

-- private
egAddNode :: (Eq (f EClassId), Hashable (f EClassId)) => f EClassId -> EGM d f p (Changed, EClassId)
egAddNode fc = do
  (c, n) <- stateLens egNodeAssocL (assocEnsure fc)
  x <- case c of
        ChangedNo -> do
          hc <- gets egHashCons
          -- partial: should exist in hashcons by construction (next case)
          pure (hc HashMap.! n)
        ChangedYes -> do
          x <- stateLens egSourceL sourceAdd
          stateLens egHashConsL (modify' (HashMap.insert n x))
          pure x
  pure (c, x)

egAddTerm :: (RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => t -> EGM d f p (Changed, EClassId)
egAddTerm = foldWholeTrackM egAddNode

egMerge :: EClassId -> EClassId -> EGM d f p (Maybe (Changed, EClassId))
egMerge i j = do
  mx <- stateLens egUnionFindL (ufMerge i j)
  case mx of
    Just (ChangedYes, x) -> stateLens egWorkListL (modify' (HashSet.insert x))
    _ -> pure ()
  pure mx

egNeedsRebuild :: EGraph d f -> Bool
egNeedsRebuild = not . HashSet.null . egWorkList

egRebuild :: EGM d f p ()
egRebuild = error "TODO"
