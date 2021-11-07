module Overeasy.IntLike.Set
  ( IntLikeSet (..)
  , empty
  , singleton
  , fromList
  , size
  , null
  , member
  , toList
  , insert
  , delete
  , minView
  , insertState
  , orderedPairs
  , unorderedPairs
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Prelude hiding (null)

newtype IntLikeSet x = IntLikeSet { unIntLikeSet :: IntSet }
  deriving stock (Show)
  deriving newtype (Eq, NFData, Semigroup, Monoid)

empty :: IntLikeSet x
empty = IntLikeSet IntSet.empty
{-# INLINE empty #-}

singleton :: Coercible x Int => x -> IntLikeSet x
singleton = IntLikeSet . IntSet.singleton . coerce
{-# INLINE singleton #-}

fromList :: Coercible x Int => [x] -> IntLikeSet x
fromList = IntLikeSet . IntSet.fromList . coerce
{-# INLINE fromList #-}

size :: IntLikeSet x -> Int
size = IntSet.size . unIntLikeSet
{-# INLINE size #-}

null :: IntLikeSet x -> Bool
null = IntSet.null . unIntLikeSet
{-# INLINE null #-}

member :: Coercible x Int => x -> IntLikeSet x -> Bool
member x = IntSet.member (coerce x) . unIntLikeSet
{-# INLINE member #-}

toList :: Coercible x Int => IntLikeSet x -> [x]
toList = coerce . IntSet.toList . unIntLikeSet
{-# INLINE toList #-}

insert :: Coercible x Int => x -> IntLikeSet x -> IntLikeSet x
insert x = IntLikeSet . IntSet.insert (coerce x) . unIntLikeSet
{-# INLINE insert #-}

delete :: Coercible x Int => x -> IntLikeSet x -> IntLikeSet x
delete x = IntLikeSet . IntSet.delete (coerce x) . unIntLikeSet
{-# INLINE delete #-}

minView :: Coercible x Int => IntLikeSet x -> Maybe (x, IntLikeSet x)
minView = coerce . IntSet.minView . unIntLikeSet
{-# INLINE minView #-}

insertState :: Coercible x Int => (Bool -> b) -> x -> IntLikeSet x -> (b, IntLikeSet x)
insertState f x = coerce . IntSet.alterF (\b -> (f b, True)) (coerce x) . unIntLikeSet
{-# INLINE insertState #-}

orderedPairs :: Coercible x Int => IntLikeSet x -> [(x, x)]
orderedPairs s = let vs = toList s in [(x, y) | x <- vs, y <- vs]

unorderedPairs :: Coercible x Int => IntLikeSet x -> [(x, x)]
unorderedPairs = go1 . toList where
  go1 vs =
    case vs of
      [] -> []
      x:xs -> go2 x xs xs
  go2 x vl vs =
    case vl of
      [] -> go1 vs
      y:vl' -> (x, y):go2 x vl' vs
