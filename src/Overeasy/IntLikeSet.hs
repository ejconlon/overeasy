module Overeasy.IntLikeSet
  ( IntLikeSet (..)
  , emptyIntLikeSet
  , singletonIntLikeSet
  , fromListIntLikeSet
  , sizeIntLikeSet
  , nullIntLikeSet
  , memberIntLikeSet
  , toListIntLikeSet
  , insertIntLikeSet
  , deleteIntLikeSet
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype IntLikeSet x = IntLikeSet { unIntLikeSet :: IntSet }
  deriving newtype (Eq, Show, NFData, Semigroup, Monoid)

emptyIntLikeSet :: IntLikeSet x
emptyIntLikeSet = IntLikeSet IntSet.empty
{-# INLINE emptyIntLikeSet #-}

singletonIntLikeSet :: Coercible x Int => x -> IntLikeSet x
singletonIntLikeSet = IntLikeSet . IntSet.singleton . coerce
{-# INLINE singletonIntLikeSet #-}

fromListIntLikeSet :: Coercible x Int => [x] -> IntLikeSet x
fromListIntLikeSet = IntLikeSet . IntSet.fromList . coerce
{-# INLINE fromListIntLikeSet #-}

sizeIntLikeSet :: IntLikeSet x -> Int
sizeIntLikeSet = IntSet.size . unIntLikeSet
{-# INLINE sizeIntLikeSet #-}

nullIntLikeSet :: IntLikeSet x -> Bool
nullIntLikeSet = IntSet.null . unIntLikeSet
{-# INLINE nullIntLikeSet #-}

memberIntLikeSet :: Coercible x Int => x -> IntLikeSet x -> Bool
memberIntLikeSet x = IntSet.member (coerce x) . unIntLikeSet
{-# INLINE memberIntLikeSet #-}

toListIntLikeSet :: Coercible x Int => IntLikeSet x -> [x]
toListIntLikeSet = coerce . IntSet.toList . unIntLikeSet
{-# INLINE toListIntLikeSet #-}

insertIntLikeSet :: Coercible x Int => x -> IntLikeSet x -> IntLikeSet x
insertIntLikeSet x = IntLikeSet . IntSet.insert (coerce x) . unIntLikeSet
{-# INLINE insertIntLikeSet #-}

deleteIntLikeSet :: Coercible x Int => x -> IntLikeSet x -> IntLikeSet x
deleteIntLikeSet x = IntLikeSet . IntSet.delete (coerce x) . unIntLikeSet
{-# INLINE deleteIntLikeSet #-}
