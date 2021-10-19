module Overeasy.IntLikeMap
  ( IntLikeMap (..)
  , emptyIntLikeMap
  , fromListIntLikeMap
  , sizeIntLikeMap
  , nullIntLikeMap
  , memberIntLikeMap
  , toListIntLikeMap
  , lookupIntLikeMap
  , partialLookupIntLikeMap
  , insertIntLikeMap
  , adjustIntLikeMap
  , deleteIntLikeMap
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

newtype IntLikeMap x a = IntLikeMap { unIntLikeMap :: IntMap a }
  deriving newtype (Eq, Show, Functor, Foldable, NFData, Semigroup, Monoid)

instance Traversable (IntLikeMap a) where
  traverse f (IntLikeMap m) = fmap IntLikeMap (traverse f m)

emptyIntLikeMap :: IntLikeMap x a
emptyIntLikeMap = IntLikeMap IntMap.empty
{-# INLINE emptyIntLikeMap #-}

fromListIntLikeMap :: Coercible x Int => [(x, a)] -> IntLikeMap x a
fromListIntLikeMap = IntLikeMap . IntMap.fromList . fmap coerce
{-# INLINE fromListIntLikeMap #-}

sizeIntLikeMap :: IntLikeMap x a -> Int
sizeIntLikeMap = IntMap.size . unIntLikeMap
{-# INLINE sizeIntLikeMap #-}

nullIntLikeMap :: IntLikeMap x a -> Bool
nullIntLikeMap = IntMap.null . unIntLikeMap
{-# INLINE nullIntLikeMap #-}

memberIntLikeMap :: Coercible x Int => x -> IntLikeMap x a -> Bool
memberIntLikeMap x = IntMap.member (coerce x) . unIntLikeMap
{-# INLINE memberIntLikeMap #-}

toListIntLikeMap :: Coercible x Int => IntLikeMap x a -> [(x, a)]
toListIntLikeMap = fmap coerce . IntMap.toList . unIntLikeMap
{-# INLINE toListIntLikeMap #-}

lookupIntLikeMap :: Coercible x Int => x -> IntLikeMap x a -> Maybe a
lookupIntLikeMap x = IntMap.lookup (coerce x) . unIntLikeMap
{-# INLINE lookupIntLikeMap #-}

partialLookupIntLikeMap :: Coercible x Int => x -> IntLikeMap x a -> a
partialLookupIntLikeMap x m = unIntLikeMap m IntMap.! coerce x
{-# INLINE partialLookupIntLikeMap #-}

insertIntLikeMap :: Coercible x Int => x -> a -> IntLikeMap x a -> IntLikeMap x a
insertIntLikeMap x a = IntLikeMap . IntMap.insert (coerce x) a . unIntLikeMap
{-# INLINE insertIntLikeMap #-}

adjustIntLikeMap :: Coercible x Int => (a -> a) -> x -> IntLikeMap x a -> IntLikeMap x a
adjustIntLikeMap f x = IntLikeMap . IntMap.adjust f (coerce x) . unIntLikeMap
{-# INLINE adjustIntLikeMap #-}

deleteIntLikeMap :: Coercible x Int => x -> IntLikeMap x a -> IntLikeMap x a
deleteIntLikeMap x = IntLikeMap . IntMap.delete (coerce x) . unIntLikeMap
{-# INLINE deleteIntLikeMap #-}
