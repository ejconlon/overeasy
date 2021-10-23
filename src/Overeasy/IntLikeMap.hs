module Overeasy.IntLikeMap
  ( IntLikeMap (..)
  , empty
  , singleton
  , fromList
  , size
  , null
  , member
  , toList
  , keys
  , lookup
  , partialLookup
  , insert
  , insertWith
  , adjust
  , delete
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Prelude hiding (lookup, null)

newtype IntLikeMap x a = IntLikeMap { unIntLikeMap :: IntMap a }
  deriving stock (Show, Traversable)
  deriving newtype (Eq, Functor, Foldable, NFData, Semigroup, Monoid)

empty :: IntLikeMap x a
empty = IntLikeMap IntMap.empty
{-# INLINE empty #-}

singleton :: Coercible x Int => x -> a -> IntLikeMap x a
singleton x = IntLikeMap . IntMap.singleton (coerce x)
{-# INLINE singleton #-}

fromList :: Coercible x Int => [(x, a)] -> IntLikeMap x a
fromList = IntLikeMap . IntMap.fromList . coerce
{-# INLINE fromList #-}

size :: IntLikeMap x a -> Int
size = IntMap.size . unIntLikeMap
{-# INLINE size #-}

null :: IntLikeMap x a -> Bool
null = IntMap.null . unIntLikeMap
{-# INLINE null #-}

member :: Coercible x Int => x -> IntLikeMap x a -> Bool
member x = IntMap.member (coerce x) . unIntLikeMap
{-# INLINE member #-}

toList :: Coercible x Int => IntLikeMap x a -> [(x, a)]
toList = coerce . IntMap.toList . unIntLikeMap
{-# INLINE toList #-}

keys :: Coercible x Int => IntLikeMap x a -> [x]
keys = coerce . IntMap.keys . unIntLikeMap
{-# INLINE keys #-}

lookup :: Coercible x Int => x -> IntLikeMap x a -> Maybe a
lookup x = IntMap.lookup (coerce x) . unIntLikeMap
{-# INLINE lookup #-}

partialLookup :: Coercible x Int => x -> IntLikeMap x a -> a
partialLookup x m = unIntLikeMap m IntMap.! coerce x
{-# INLINE partialLookup #-}

insert :: Coercible x Int => x -> a -> IntLikeMap x a -> IntLikeMap x a
insert x a = IntLikeMap . IntMap.insert (coerce x) a . unIntLikeMap
{-# INLINE insert #-}

insertWith :: Coercible x Int => (a -> a -> a) -> x -> a -> IntLikeMap x a -> IntLikeMap x a
insertWith f x a = IntLikeMap . IntMap.insertWith f (coerce x) a . unIntLikeMap
{-# INLINE insertWith #-}

adjust :: Coercible x Int => (a -> a) -> x -> IntLikeMap x a -> IntLikeMap x a
adjust f x = IntLikeMap . IntMap.adjust f (coerce x) . unIntLikeMap
{-# INLINE adjust #-}

delete :: Coercible x Int => x -> IntLikeMap x a -> IntLikeMap x a
delete x = IntLikeMap . IntMap.delete (coerce x) . unIntLikeMap
{-# INLINE delete #-}
