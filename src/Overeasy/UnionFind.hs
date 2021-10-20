{-# LANGUAGE DeriveAnyClass #-}

-- | A Union-Find implementation.
-- Not the best - requires at least 2 lookups to find the root.
-- But at least it's a persistent impl, and it compresses paths as it goes.
module Overeasy.UnionFind
  ( ufOnConflict
  , UnionFind
  , ufSize
  , ufTotalSize
  , ufNew
  , ufMembers
  , ufRoots
  , ufAdd
  , ufFind
  , ufPartialFind
  , MergeRes (..)
  , ufMerge
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, modify', state)
import Data.Coerce (Coercible, coerce)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Overeasy.IntLikeMap (IntLikeMap, emptyIntLikeMap, insertIntLikeMap, keysIntLikeMap, lookupIntLikeMap,
                            memberIntLikeMap, partialLookupIntLikeMap, sizeIntLikeMap)
import Overeasy.IntLikeSet (IntLikeSet, emptyIntLikeSet, insertIntLikeSet, singletonIntLikeSet)
import Overeasy.StateUtil (stateFail)

-- | Our default choice for merging class ids.
ufOnConflict :: Ord x => x -> x -> x
ufOnConflict = min
{-# INLINE ufOnConflict #-}

-- private ctor
data UnionFind x = UnionFind
  { ufSize :: !Int  -- ^ How many classes are there?
  , ufParents :: !(IntLikeMap x x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many discrete members have ever been added? (Number of classes via 'ufSize' is always LTE total.)
ufTotalSize :: UnionFind x -> Int
ufTotalSize = sizeIntLikeMap . ufParents

-- | Creates a new UF
ufNew :: UnionFind x
ufNew = UnionFind 0 emptyIntLikeMap

-- private
ufMembersInc :: (Coercible x Int, Eq x) => UnionFind x -> (IntLikeMap x (IntLikeSet x), UnionFind x)
ufMembersInc u@(UnionFind _ p) = foldr go (emptyIntLikeMap, u) (keysIntLikeMap p) where
  go x1 (m, v) =
    let (x2, v') = ufFindRootInc v x1
        m' = insertIntLikeMap x2 (maybe (singletonIntLikeSet x1) (insertIntLikeSet x1) (lookupIntLikeMap x2 m)) m
    in (m', v')

-- | Enumerates the members of the UF per-class (keys are roots)
ufMembers :: (Coercible x Int, Eq x) => State (UnionFind x) (IntLikeMap x (IntLikeSet x))
ufMembers = state ufMembersInc

-- private
ufRootsInc :: (Coercible x Int, Eq x) => UnionFind x -> (IntLikeSet x, UnionFind x)
ufRootsInc u@(UnionFind _ p) = foldr go (emptyIntLikeSet, u) (keysIntLikeMap p) where
  go x1 (s, v) =
    let (x2, v') = ufFindRootInc v x1
        s' = insertIntLikeSet x2 s
    in (s', v')

-- | Enumerates the roots of the UF
ufRoots :: (Coercible x Int, Eq x) => State (UnionFind x) (IntLikeSet x)
ufRoots = state ufRootsInc

-- private
ufAddInc :: Coercible x Int => x -> UnionFind x -> UnionFind x
ufAddInc x u@(UnionFind z p) =
  if memberIntLikeMap x p
    then u
    else UnionFind (z + 1) (insertIntLikeMap x x p)

-- | Adds a new member to the UF
ufAdd :: Coercible x Int => x -> State (UnionFind x) ()
ufAdd = modify' . ufAddInc

-- private
ufFindRootAcc :: (Coercible x Int, Eq x) => IntLikeMap x x -> [x] -> x -> ([x], x)
ufFindRootAcc p acc x1 =
  -- partial: should exist in map by construction (all xs added in ufAddInc)
  let x2 = partialLookupIntLikeMap x1 p
  in if x1 == x2
    then (acc, x2)
    else ufFindRootAcc p (x1:acc) x2

-- private
ufFindRootInc :: (Coercible x Int, Eq x) => UnionFind x -> x -> (x, UnionFind x)
ufFindRootInc u@(UnionFind z p) x1 =
  let (acc, x2) = ufFindRootAcc p [] x1
      u' = case acc of
            [] -> u
            _ -> let p' = foldr (`insertIntLikeMap` x2) p acc
                 in UnionFind z p'
  in (x2, u')

-- private
ufFindInc :: (Coercible x Int, Eq x) => x -> UnionFind x -> Maybe (x, UnionFind x)
ufFindInc a u@(UnionFind _ p) = fmap (ufFindRootInc u) (lookupIntLikeMap a p)

-- | Finds the canonical class member of the UF or 'Nothing' if not found
ufFind :: (Coercible x Int, Eq x) => x -> State (UnionFind x) (Maybe x)
ufFind x = stateFail (ufFindInc x)

-- | Finds the canonical class member of the UF or calls 'error'.
-- NOTE: THIS IS PARTIAL!
ufPartialFind :: (Coercible x Int, Eq x) => x -> State (UnionFind x) x
ufPartialFind x = fmap (fromMaybe (error ("Could not find in UF: " ++ show (coerce x :: Int)))) (ufFind x)

-- | The result of trying to merge two elements of the 'UnionFind'
data MergeRes x =
    MergeResMissing !x
  | MergeResUnchanged !x
  | MergeResChanged !x !x !x  -- ^ leftRoot rightRoot newRoot
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- private
ufMergeInc :: (Coercible x Int, Ord x) => x -> x -> UnionFind x -> (MergeRes x, UnionFind x)
ufMergeInc i j u@(UnionFind z p) = finalRes where
  finalRes =
    if memberIntLikeMap i p
      then if memberIntLikeMap j p
        then go i j
        else (MergeResMissing j, u)
      else (MergeResMissing i, u)
  go ix1 jx1 =
    let (iacc, ix2) = ufFindRootAcc p [] ix1
        (acc, jx2) = ufFindRootAcc p iacc jx1
    in if ix2 == jx2
      then
        let res = MergeResUnchanged ix2
        in case acc of
          [] -> (res, u)
          _ -> let p' = foldr (`insertIntLikeMap` ix2) p acc
              in (res, UnionFind z p')
      else
        let kx = ufOnConflict ix2 jx2
            kacc
              | ix2 < jx2 = if jx1 == jx2 then jx1:acc else jx2:jx1:acc
              | otherwise = if ix1 == ix2 then ix1:acc else ix2:ix1:acc
            p' = foldr (`insertIntLikeMap` kx) p kacc
        in (MergeResChanged ix2 jx2 kx, UnionFind (z - 1) p')

-- | Merge two classes in the UF
ufMerge :: (Coercible x Int, Ord x) => x -> x -> State (UnionFind x) (MergeRes x)
ufMerge i j = state (ufMergeInc i j)
