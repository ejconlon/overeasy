{-# LANGUAGE DeriveAnyClass #-}

module Overeasy where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

-- class (Traversable p, Traversable f) => Unifiable p f | p -> f where
--   zipMatch :: p a -> f b -> Maybe (p (a, b))

-- data PatternF r f a =
--     PatternWildF
--   | PatternVarF !a
--   | PatternEmbedF !(f r)
--   deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
--   deriving anyclass (Hashable, NFData)

-- newtype Pattern f a = Pattern { unPattern :: PatternF (Pattern f a) f a }

-- newtype Match v f a = Match { unMatch :: PatternF v f a }

data ArithF a =
    ArithPlusF a a
  | ArithTimesF a a
  | ArithShiftLF a !Int
  | ArithShiftRF a !Int
  | ArithConstF !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

newtype Arith = Arith { unArith :: ArithF Arith }
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

newtype Partition = Partition { unPartition :: Int } deriving newtype (Eq, Ord, Enum, Show, Hashable, NFData)

data UnionFind a = UnionFind
  { ufNextPartition :: !Partition
  , ufSize :: !Int
  , ufParents :: !(HashMap Partition Partition)
  , ufClasses :: !(HashMap a Partition)
  }

ufNew :: UnionFind a
ufNew = UnionFind (Partition 0) 0 HashMap.empty HashMap.empty

ufMembers :: (Eq a, Hashable a) => UnionFind a -> HashMap Partition (HashSet a)
ufMembers u = foldr go HashMap.empty (HashMap.toList (ufClass u)) where
  go (a, x1) m =
    let x2 = ufParent u HashMap.! x1
    in record x2 a m
  record x a m = HashMap.insert x (maybe (HashSet.singleton a) (HashSet.insert a) (HashMap.lookup x m)) m

ufMembersInc :: (Eq a, Hashable a) => UnionFind a -> (HashMap Partition (HashSet a), UnionFind a)
ufMembersInc u = foldr go (HashMap.empty, u) (HashMap.toList (ufClass u)) where
  go (a, x1) (m, v) =
    let x2 = ufParent v HashMap.! x1
        m' =  record x2 a m
        v' = if x1 == x2 then v else v { ufClass = HashMap.insert a x2 (ufClass v) }
    in (m', v')
  record x a m = HashMap.insert x (maybe (HashSet.singleton a) (HashSet.insert a) (HashMap.lookup x m)) m

ufAdd :: (Eq a, Hashable a) => a -> UnionFind a -> (Partition, UnionFind a)
ufAdd a (UnionFind s z p c) = (s, UnionFind (succ s) (succ z) (HashMap.insert s s p) (HashMap.insert a s c))

ufFind :: (Eq a, Hashable a) => a -> UnionFind a -> Maybe Partition
ufFind a (UnionFind _ _ p c) = HashMap.lookup a c >>= \x -> HashMap.lookup x p

ufFindInc :: (Eq a, Hashable a) => a -> UnionFind a -> Maybe (Partition, UnionFind a)
ufFindInc a u@(UnionFind s z p c) = do
  x1 <- HashMap.lookup a c
  x2 <- HashMap.lookup x1 p
  let u' = if x1 == x2 then u else UnionFind s z p (HashMap.insert a x2 c)
  pure (x2, u')

ufMerge :: (Eq a, Hashable a) => a -> a -> UnionFind a -> Maybe (UnionFind a)
ufMerge i j u = do
  (ix, u') <- ufFindInc i u
  (jx, u'') <- ufFindInc i u'
  if ix == jx
    then pure u''
    else
      let UnionFind s z p c = u''
      in if ix < jx
        then pure (UnionFind s (pred z) (HashMap.insert jx ix p) (HashMap.insert j ix c))
        else pure (UnionFind s (pred z) (HashMap.insert ix jx p) (HashMap.insert i jx c))

-- data EGraph a = EGraph
--   {
--   }
