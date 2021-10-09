{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Overeasy.EGraph
  ( EClassId
  , ENodeId
  , EGraph
  , egNew
  , egCanonicalize
  , egAddTerm
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, gets, modify')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesFor)
import Overeasy.Assoc (Assoc, AssocEnsureResult (..), assocEnsure, assocNew)
import Overeasy.Recursion (RecursiveWhole, foldWholeM)
import Overeasy.Source (Source, sourceAdd, sourceNew)
import Overeasy.StateUtil (stateLens)
import Overeasy.UnionFind (UnionFind, ufFind, ufNew)

newtype EClassId = EClassId { unEClassId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

newtype ENodeId = ENodeId { unENodeId :: Int } deriving newtype (Eq, Ord, Show, Enum, Hashable, NFData)

-- private ctor
data EGraph f = EGraph
  { egSource :: !(Source EClassId)
  , egUnionFind :: !(UnionFind EClassId)
  , egClassMap :: !(HashMap EClassId (HashSet ENodeId))
  , egNodeAssoc :: !(Assoc ENodeId (f EClassId))
  , egHashCons :: !(HashMap ENodeId EClassId)
  } deriving stock (Generic)

deriving stock instance Eq (f EClassId) => Eq (EGraph f)
deriving stock instance Show (f EClassId) => Show (EGraph f)
deriving anyclass instance NFData (f EClassId) => NFData (EGraph f)

makeLensesFor
  [ ("egSource", "egSourceL")
  , ("egUnionFind", "egUnionFindL")
  , ("egClassMap", "egClassMapL")
  , ("egNodeAssoc", "egNodeAssocL")
  , ("egHashCons", "egHashConsL")
  ] ''EGraph

egNew :: EGraph f
egNew = EGraph (sourceNew (EClassId 0)) ufNew HashMap.empty (assocNew (ENodeId 0)) HashMap.empty

egCanonicalize :: Traversable f => f EClassId -> State (EGraph f) (Maybe (f EClassId))
egCanonicalize = stateLens egUnionFindL . fmap sequence . traverse ufFind

-- private
egAddNode :: (Eq (f EClassId), Hashable (f EClassId)) => f EClassId -> State (EGraph f) EClassId
egAddNode fc = do
  (n, r) <- stateLens egNodeAssocL (assocEnsure fc)
  case r of
    AssocEnsureExists -> do
      hc <- gets egHashCons
      pure (hc HashMap.! n)
    AssocEnsureAdded -> do
      c <- stateLens egSourceL sourceAdd
      stateLens egHashConsL (modify' (HashMap.insert n c))
      pure c

egAddTerm :: (RecursiveWhole t f, Traversable f, Eq (f EClassId), Hashable (f EClassId)) => t -> State (EGraph f) EClassId
egAddTerm = foldWholeM egAddNode
