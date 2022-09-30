{-# LANGUAGE DeriveAnyClass #-}

-- | Associates elements with unique ids drawn from a 'Source'
module Overeasy.Assoc
  ( Assoc
  , assocFwd
  , assocBwd
  , assocEquiv
  , assocSize
  , assocNew
  , assocSingleton
  , AssocInsertRes (..)
  , assocInsertInc
  , assocInsert
  , assocFromList
  , assocToList
  , assocMember
  , assocLookupByKey
  , assocPartialLookupByKey
  , assocLookupByValue
  , assocPartialLookupByValue
  , assocLookupRoot
  , assocRoots
  , assocLeaves
  , assocMembers
  , assocCanCompact
  , assocCompactInc
  , assocCompact
  , assocRemoveAllInc
  , assocRemoveAll
  , assocUnion
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState (..), State, modify')
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import Overeasy.EquivFind (EquivAddRes (..), EquivFind, efAddInc, efBwd, efCanCompact, efCompactInc, efLeaves,
                           efLookupRoot, efMember, efMembers, efNew, efRemoveAllInc, efRoots, efSingleton,
                           efUnsafeAddLeafInc, efUnsafeMerge)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(IntLikeMap x a)
  , assocBwd :: !(HashMap a x)
  , assocEquiv :: !(EquivFind x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many elements are still in the map?
assocSize :: Assoc x a -> Int
assocSize = ILM.size . assocFwd

-- | Creates an empty 'Assoc'
assocNew :: Assoc x a
assocNew = Assoc ILM.empty HashMap.empty efNew

-- | Creates a singleton 'Assoc'
assocSingleton :: (Coercible x Int, Hashable a) => x -> a -> Assoc x a
assocSingleton x a = Assoc (ILM.singleton x a) (HashMap.singleton a x) (efSingleton x)

data AssocInsertRes x =
    AssocInsertResUnchanged
  | AssocInsertResCreated
  | AssocInsertResUpdated
  | AssocInsertResMerged !(IntLikeSet x)
  deriving stock (Eq, Show)

assocInsertInc :: (Coercible x Int, Ord x, Eq a, Hashable a) => x -> a -> Assoc x a -> ((x, AssocInsertRes x), Assoc x a)
assocInsertInc x a1 assoc@(Assoc fwd bwd equiv) = finalRes where
  finalRes =
    let (res, equiv') = efAddInc x equiv
    in case res of
      EquivAddResNewRoot -> insertRoot x equiv'
      EquivAddResAlreadyLeafOf z -> updateRoot z
      EquivAddResAlreadyRoot -> updateRoot x
  updateRoot w =
    -- w is existing root and is guaranteed to map to something
    let a0 = ILM.partialLookup w fwd
    in if a0 == a1
      -- the value has not changed, don't need to change assoc
      then ((w, AssocInsertResUnchanged), assoc)
      else
        -- value has changed, need to check if it's fresh
        case HashMap.lookup a1 bwd of
          -- never seen; insert and return
          Nothing ->
            let fwd' = ILM.insert w a1 fwd
                bwd' = HashMap.insert a1 w (HashMap.delete a0 bwd)
            in ((w, AssocInsertResUpdated), Assoc fwd' bwd' equiv)
          -- mapped to another set of nodes, merge
          Just v ->
            let (toKeep, toDelete, equiv') = efUnsafeMerge w v equiv
                res = AssocInsertResMerged toDelete
            in if toKeep == w
              -- w wins
              then
                let fwd' = ILM.insert w a1 (ILM.delete v fwd)
                    bwd' = HashMap.insert a1 w (HashMap.delete a0 bwd)
                in ((w, res), Assoc fwd' bwd' equiv')
              -- v wins
              else
                let fwd' = ILM.delete w fwd
                    bwd' = HashMap.delete a0 bwd
                in ((v, res), Assoc fwd' bwd' equiv')
  insertRoot w equiv' =
    -- w is new root that doesn't exist
    case HashMap.lookup a1 bwd of
      -- never seen; insert and return
      Nothing ->
        let fwd' = ILM.insert w a1 fwd
            bwd' = HashMap.insert a1 w bwd
        in ((w, AssocInsertResCreated), Assoc fwd' bwd' equiv')
      Just v ->
        let (toKeep, toDelete, equiv'') = efUnsafeMerge w v equiv'
            res = AssocInsertResMerged toDelete
        in if toKeep == w
          -- w wins
          then
            let fwd' = ILM.insert w a1 (ILM.delete v fwd)
                bwd' = HashMap.insert a1 w bwd
            in ((w, res), Assoc fwd' bwd' equiv'')
          -- v wins
          else
            let fwd' = ILM.delete w fwd
            in ((v, res), Assoc fwd' bwd equiv'')

assocInsert :: (Coercible x Int, Ord x, Eq a, Hashable a) => x -> a -> State (Assoc x a) (x, AssocInsertRes x)
assocInsert x a = state (assocInsertInc x a)

assocFromList :: (Coercible x Int, Ord x, Eq a, Hashable a) => [(x, a)] -> Assoc x a
assocFromList = foldl' (\assoc (x, a) -> snd (assocInsertInc x a assoc)) assocNew

assocToList :: Coercible x Int => Assoc x a -> [(x, a)]
assocToList = ILM.toList . assocFwd

assocMember :: Coercible x Int => x -> Assoc x a -> Bool
assocMember x (Assoc _ _ equiv) = efMember x equiv

-- | Forward lookup
assocLookupByKey :: Coercible x Int => x -> Assoc x a -> Maybe a
assocLookupByKey x (Assoc fwd _ equiv) = ILM.lookup (efLookupRoot x equiv) fwd

-- | PARTIAL forward lookup
assocPartialLookupByKey :: Coercible x Int => x -> Assoc x a -> a
assocPartialLookupByKey x = fromJust . assocLookupByKey x

-- | Backward lookup
assocLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> Maybe x
assocLookupByValue a = HashMap.lookup a . assocBwd

-- | PARTIAL backward lookup
assocPartialLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> x
assocPartialLookupByValue a = flip (HashMap.!) a . assocBwd

-- | Finds the root for the given key (id if not found)
assocLookupRoot :: Coercible x Int => x -> Assoc x a -> x
assocLookupRoot x = efLookupRoot x . assocEquiv

-- | List all root (live, non-compactible) entries
assocRoots :: Coercible x Int => Assoc x a -> [x]
assocRoots = efRoots . assocEquiv

-- | List all leaf (dead, compactible) entries
assocLeaves :: Coercible x Int => Assoc x a -> [x]
assocLeaves = efLeaves . assocEquiv

assocMembers :: Coercible x Int => Assoc x a -> [x]
assocMembers = efMembers . assocEquiv

-- | Are there dead keys in the equiv from 'assocInsert'?
assocCanCompact :: Assoc x a -> Bool
assocCanCompact = efCanCompact . assocEquiv

assocCompactInc :: Coercible x Int => Assoc x a -> (IntLikeMap x x, Assoc x a)
assocCompactInc assoc@(Assoc fwd bwd equiv) =
  let replacements = efBwd equiv
      assoc' =
        if ILM.null replacements
          then assoc
          else let (_, equiv') = efCompactInc equiv in Assoc fwd bwd equiv'
  in (replacements, assoc')

-- | Removes all dead keys in the equiv
-- Returns map of dead leaf node -> live root node
assocCompact :: Coercible x Int => State (Assoc x a) (IntLikeMap x x)
assocCompact = state assocCompactInc

assocRemoveAllInc :: (Coercible x Int, Eq a, Hashable a) => [x] -> Assoc x a -> Assoc x a
assocRemoveAllInc xs (Assoc fwd0 bwd0 equiv0) = Assoc fwdFinal bwdFinal equivFinal where
  (remap, equivFinal) = efRemoveAllInc xs equiv0
  (fwdFinal, bwdFinal) = foldl' go (fwd0, bwd0) xs
  go tup@(fwd, bwd) x =
    case ILM.lookup x fwd of
      -- Leaf, ignore
      Nothing -> tup
      -- Root
      Just a ->
        case ILM.lookup x remap of
          -- Singleton root, delete
          Nothing ->
            let fwd' = ILM.delete x fwd
                bwd' = HashMap.delete a bwd
            in (fwd', bwd')
          -- Remapped root, rotate
          Just y ->
            let fwd' = ILM.delete x (ILM.insert y a fwd)
                bwd' = HashMap.insert a y bwd
            in (fwd', bwd')

-- | Removes the given keys from the assoc.
-- Values will only be removed from the assoc if the key is a singleton root.
-- If a key is not found, it is simply ignored.
assocRemoveAll :: (Coercible x Int, Eq a, Hashable a) => [x] -> State (Assoc x a) ()
assocRemoveAll = modify' . assocRemoveAllInc

-- | Join two assocs (uses the first as the base)
assocUnion :: (Coercible x Int, Ord x, Eq a, Hashable a) => Assoc x a -> Assoc x a -> Assoc x a
assocUnion base (Assoc fwd _ equiv) = Assoc fwdFinal bwdFinal equivFinal where
  goRoots assocGo (x, a) = snd (assocInsertInc x a assocGo)
  goLeaves equivGo (leaf, oldRoot) = efUnsafeAddLeafInc oldRoot leaf equivGo
  Assoc fwdFinal bwdFinal equivMid = foldl' goRoots base (ILM.toList fwd)
  equivFinal = foldl' goLeaves equivMid (ILM.toList (efBwd equiv))
