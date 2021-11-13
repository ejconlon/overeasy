module Overeasy.IntLike.MultiMap
  ( IntLikeMultiMap
  , empty
  , size
  , toList
  , insert
  , member
  , invertDisjoint
  , fromInvertedMap
  ) where

import Control.Monad (foldM)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS

type IntLikeMultiMap k v = IntLikeMap k (IntLikeSet v)

empty :: IntLikeMultiMap k v
empty = ILM.empty
{-# INLINE empty #-}

size :: IntLikeMultiMap k v -> Int
size = ILM.size
{-# INLINE size #-}

toList :: Coercible k Int => IntLikeMultiMap k v -> [(k, IntLikeSet v)]
toList = ILM.toList
{-# INLINE toList #-}

insert :: (Coercible k Int, Coercible v Int) => k -> v -> IntLikeMultiMap k v -> IntLikeMultiMap k v
insert k v = ILM.insertWith ILS.union k (ILS.singleton v)
{-# INLINE insert #-}

member :: (Coercible k Int, Coercible v Int) => k -> v -> IntLikeMultiMap k v -> Bool
member k v = maybe False (ILS.member v) . ILM.lookup k
{-# INLINE member #-}

invertDisjoint :: (Coercible k Int, Coercible v Int) => IntLikeMultiMap k v -> Either (k, k, v) (IntLikeMap v k)
invertDisjoint = foldM go1 ILM.empty . ILM.toList where
  go1 m (k, vs) = foldM (go2 k) m (ILS.toList vs)
  go2 k m v =
    case ILM.lookup v m of
      Nothing -> Right (ILM.insert v k m)
      Just k' -> Left (k, k', v)

fromInvertedMap :: (Coercible k Int, Coercible v Int) => IntLikeMap k v -> IntLikeMultiMap v k
fromInvertedMap = foldl' (\m (k, v) -> insert v k m) empty . ILM.toList
{-# INLINE fromInvertedMap #-}
