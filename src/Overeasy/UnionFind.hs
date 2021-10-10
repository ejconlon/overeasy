{-# LANGUAGE DeriveAnyClass #-}

-- | A Union-Find implementation.
-- Not the best - requires at least 2 lookups to find the root.
-- But at least it's a persistent impl, and it compresses paths as it goes.
module Overeasy.UnionFind
  ( UnionFind
  , ufSize
  , ufNew
  , ufMembers
  , ufAdd
  , ufFind
  , ufMerge
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, modify', state)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.StateUtil (Changed (..), stateFail)

-- private ctor
data UnionFind x = UnionFind
  { ufSize :: !Int  -- ^ How many classes are there?
  , ufParents :: !(HashMap x x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many discrete members have ever been added? (Number of classes via 'ufSize' is always LTE.)
ufTotalSize :: UnionFind x -> Int
ufTotalSize = HashMap.size . ufParents

-- | Creates a new UF
ufNew :: UnionFind x
ufNew = UnionFind 0 HashMap.empty

-- private
ufMembersInc :: (Eq x, Hashable x) => UnionFind x -> (HashMap x (HashSet x), UnionFind x)
ufMembersInc u@(UnionFind _ p) = foldr go (HashMap.empty, u) (HashMap.keys p) where
  go x1 (m, v) =
    let (x2, v') = ufFindRootInc v x1
        m' = HashMap.insert x2 (maybe (HashSet.singleton x1) (HashSet.insert x1) (HashMap.lookup x2 m)) m
    in (m', v')

-- | Enumerates the members of the UF per-class
ufMembers :: (Eq x, Hashable x) => State (UnionFind x) (HashMap x (HashSet x))
ufMembers = state ufMembersInc

-- private
ufAddInc :: (Eq x, Hashable x) => x -> UnionFind x -> UnionFind x
ufAddInc x u@(UnionFind z p) =
  if HashMap.member x p
    then u
    else UnionFind (z + 1) (HashMap.insert x x p)

-- | Adds a new member to the UF
ufAdd :: (Eq x, Hashable x) => x -> State (UnionFind x) ()
ufAdd = modify' . ufAddInc

-- private
ufFindRootAcc :: (Eq x, Hashable x) => HashMap x x -> [x] -> x -> ([x], x)
ufFindRootAcc p acc x1 =
  -- partial: should exist in map by construction (all xs added in ufAddInc)
  let x2 = p HashMap.! x1
  in if x1 == x2
    then (acc, x2)
    else ufFindRootAcc p (x1:acc) x2

-- private
ufFindRootInc :: (Eq x, Hashable x) => UnionFind x -> x -> (x, UnionFind x)
ufFindRootInc u@(UnionFind z p) x1 =
  let (acc, x2) = ufFindRootAcc p [] x1
      u' = case acc of
            [] -> u
            _ -> let p' = foldr (`HashMap.insert` x2) p acc
                 in UnionFind z p'
  in (x2, u')

-- private
ufFindInc :: (Eq x, Hashable x) => x -> UnionFind x -> Maybe (x, UnionFind x)
ufFindInc a u@(UnionFind _ p) = fmap (ufFindRootInc u) (HashMap.lookup a p)

-- | Finds the canonical class member of the UF or 'Nothing' if not found
ufFind :: (Eq x, Hashable x) => x -> State (UnionFind x) (Maybe x)
ufFind x = stateFail (ufFindInc x)

-- private
ufMergeInc :: (Ord x, Hashable x) => x -> x -> UnionFind x -> Maybe ((Changed, x), UnionFind x)
ufMergeInc i j u@(UnionFind z p) = if HashMap.member i p && HashMap.member j p then Just (go i j) else Nothing where
  go ix1 jx1 =
    let (iacc, ix2) = ufFindRootAcc p [] ix1
        (acc, jx2) = ufFindRootAcc p iacc jx1
    in if ix2 == jx2
      then case acc of
        [] -> ((ChangedNo, ix2), u)
        _ -> let p' = foldr (`HashMap.insert` ix2) p acc
             in ((ChangedNo, ix2), UnionFind z p')
      else
        let (kacc, kx) =
              if ix2 < jx2
                then (if jx1 == jx2 then jx1:acc else jx2:jx1:acc, ix2)
                else (if ix1 == ix2 then ix1:acc else ix2:ix1:acc, jx2)
            p' = foldr (`HashMap.insert` kx) p kacc
        in ((ChangedYes, kx), UnionFind (z - 1) p')

-- | Merge two classes in the UF
ufMerge :: (Ord x, Hashable x) => x -> x -> State (UnionFind x) (Maybe (Changed, x))
ufMerge i j = stateFail (ufMergeInc i j)
