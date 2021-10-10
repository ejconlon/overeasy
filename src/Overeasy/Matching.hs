-- | Methods to match patterns in 'EGraph's
module Overeasy.Matching
  (
  ) where

import Data.HashMap.Strict (HashMap)
import Overeasy.EGraph (EClassId, EGraph)

type Subst v = HashMap v EClassId

data EMatch v = EMatch
  { emClass :: !EClassId
  , emSubsts :: ![Subst v]
  }

class ESearcher d f v q | q -> d f v where
  esSearchOne :: q -> EClassId -> EGraph d f -> Maybe (EMatch v)
  esSearchMany :: q -> EGraph d f -> [EMatch v]
  -- default impl iterates through all classes

class EApplier d f v q | q -> d f v where
  epApplyOne :: q -> EClassId -> Subst v -> EGraph d f -> ([EClassId], EGraph d f)
  -- epApplyMany :: q ->
  epVars :: q -> [v]
