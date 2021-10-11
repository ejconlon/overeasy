-- | Methods to match patterns in 'EGraph's
module Overeasy.Matching
  (
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Overeasy.EGraph (EClassId, EGraph)

type Subst v = HashMap v EClassId

class ESearcher d f v q | q -> d f v where
  esSearchOne :: q -> EClassId -> EGraph d f -> Maybe (Subst v)

class EApplier d f v q | q -> d f v where
  epApplyOne :: q -> EClassId -> Subst v -> EGraph d f -> ([EClassId], EGraph d f)
  epVars :: q -> HashSet v
