module Overeasy.IntLike.Map
  ( IntLikeMap (..)
  , empty
  , singleton
  , fromList
  , size
  , null
  , member
  , toList
  , keys
  , elems
  , lookup
  , partialLookup
  , findWithDefault
  , insert
  , insertWith
  , adjust
  , delete
  , minViewWithKey
  , filter
  , map
  , insertState
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Prelude hiding (filter, lookup, map, null)

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

elems :: IntLikeMap x a -> [a]
elems = IntMap.elems . unIntLikeMap
{-# INLINE elems #-}

lookup :: Coercible x Int => x -> IntLikeMap x a -> Maybe a
lookup x = IntMap.lookup (coerce x) . unIntLikeMap
{-# INLINE lookup #-}

partialLookup :: Coercible x Int => x -> IntLikeMap x a -> a
partialLookup x m = unIntLikeMap m IntMap.! coerce x
{-# INLINE partialLookup #-}

findWithDefault :: Coercible x Int => a -> x -> IntLikeMap x a -> a
findWithDefault a x = IntMap.findWithDefault a (coerce x) . unIntLikeMap
{-# INLINE findWithDefault #-}

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

minViewWithKey :: Coercible x Int => IntLikeMap x a -> Maybe ((x, a), IntLikeMap x a)
minViewWithKey = coerce . IntMap.minViewWithKey . unIntLikeMap
{-# INLINE minViewWithKey #-}

filter :: (a -> Bool) -> IntLikeMap x a -> IntLikeMap x a
filter f = IntLikeMap . IntMap.filter f . unIntLikeMap
{-# INLINE filter #-}

map :: (a -> b) -> IntLikeMap x a -> IntLikeMap x b
map f = IntLikeMap . IntMap.map f . unIntLikeMap
{-# INLINE map #-}

insertState :: Coercible x Int => (Maybe a -> b) -> x -> a -> IntLikeMap x a -> (b, IntLikeMap x a)
insertState f x a = coerce . IntMap.alterF (\m -> (f m, Just a)) (coerce x) . unIntLikeMap
{-# INLINE insertState #-}
