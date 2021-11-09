module Overeasy.IntLike.Equiv
  ( IntLikeEquiv
  , fwdView
  , bwdView
  , empty
  , insert
  , partialInsert
  , member
  , lookupClass
  ) where

import Data.Coerce (Coercible)
import Data.Either (fromRight)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.MultiMap (IntLikeMultiMap)
import qualified Overeasy.IntLike.MultiMap as ILMM

data IntLikeEquiv k v = IntLikeEquiv
  { fwdView :: !(IntLikeMultiMap k v)
  , bwdView :: !(IntLikeMap v k)
  } deriving stock (Eq, Show)

empty :: IntLikeEquiv k v
empty = IntLikeEquiv ILMM.empty ILM.empty
{-# INLINE empty #-}

insert :: (Coercible k Int, Coercible v Int) => k -> v -> IntLikeEquiv k v -> Either k (IntLikeEquiv k v)
insert k v (IntLikeEquiv fwd bwd) =
  case ILM.lookup v bwd of
    Nothing -> Right (IntLikeEquiv (ILMM.insert k v fwd) (ILM.insert v k bwd))
    Just k' -> Left k'

partialInsert :: (Coercible k Int, Coercible v Int) => k -> v -> IntLikeEquiv k v -> IntLikeEquiv k v
partialInsert k v = fromRight (error "duplicate insert into equiv") . insert k v

member :: (Eq k, Coercible v Int) => k -> v -> IntLikeEquiv k v -> Bool
member k v m = Just k == lookupClass v m

lookupClass :: (Coercible v Int) => v -> IntLikeEquiv k v -> Maybe k
lookupClass v = ILM.lookup v . bwdView
