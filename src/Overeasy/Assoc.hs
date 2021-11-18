{-# LANGUAGE DeriveAnyClass #-}

-- | Associates elements with unique ids drawn from a 'Source'
module Overeasy.Assoc
  ( Assoc
  , assocFwd
  , assocBwd
  , assocEquiv
  , assocChanged
  , assocSize
  , assocNew
  , assocInsert
  , assocFromList
  , assocToList
  , assocLookupByKey
  , assocLookupByValue
  , assocCanCompact
  , assocCompact
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState (..), State)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.StateUtil (stateFail)
import Overeasy.EquivFind (EquivFind, EquivEnsureRes (..), efNew, efLookupRoot, efUnsafeMerge, efEnsureInc)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(IntLikeMap x a)
  , assocBwd :: !(HashMap a x)
  , assocEquiv :: !(EquivFind x)
  , assocChanged :: !(IntLikeSet x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many elements are still in the map?
assocSize :: Assoc x a -> Int
assocSize = ILM.size . assocFwd

-- | Creates a new 'Assoc'
assocNew :: Assoc x a
assocNew = Assoc ILM.empty HashMap.empty efNew ILS.empty

assocInsertInc :: (Coercible x Int, Ord x, Eq a, Hashable a) => x -> a -> Assoc x a -> (x, Assoc x a)
assocInsertInc x a1 assoc@(Assoc fwd bwd equiv changed) = finalRes where
  finalRes =
    let (res, equiv') = efEnsureInc x equiv
    in case res of
      EquivEnsureResNewRoot -> insertRoot x equiv'
      EquivEnsureResAlreadyLeafOf z -> updateRoot z
      EquivEnsureResAlreadyRoot -> updateRoot x
  updateRoot w =
    -- w is existing root and is guaranteed to map to something
    let a0 = ILM.partialLookup w fwd
    in if a0 == a1
      -- the value has not changed, don't need to change assoc
      then (w, assoc)
      else
        -- value has changed, need to check if it's fresh
        case HashMap.lookup a1 bwd of
          -- never seen; insert and return
          Nothing ->
            let fwd' = ILM.insert w a1 fwd
                bwd' = HashMap.insert a1 w (HashMap.delete a0 bwd)
                changed' = ILS.insert w changed
            in (w, Assoc fwd' bwd' equiv changed')
          -- mapped to another set of nodes, merge
          Just v ->
            let (toKeep, _, equiv') = efUnsafeMerge w v equiv
            in if toKeep == w
              -- w wins
              then
                let fwd' = ILM.insert w a1 (ILM.delete v fwd)
                    bwd' = HashMap.insert a1 w (HashMap.delete a0 bwd)
                    changed' = ILS.insert w changed
                in (w, Assoc fwd' bwd' equiv' changed')
              -- v wins
              else
                let fwd' = ILM.delete w fwd
                    bwd' = HashMap.delete a0 bwd
                    changed' = ILS.insert w changed
                in (v, Assoc fwd' bwd' equiv' changed')
  insertRoot w equiv' =
    -- w is new root that doesn't exist
    case HashMap.lookup a1 bwd of
      -- never seen; insert and return
      Nothing ->
        let fwd' = ILM.insert w a1 fwd
            bwd' = HashMap.insert a1 w bwd
            changed' = ILS.insert w changed
        in (w, Assoc fwd' bwd' equiv' changed')
      Just v ->
        let (toKeep, _, equiv'') = efUnsafeMerge w v equiv'
        in if toKeep == w
          -- w wins
          then
            let fwd' = ILM.insert w a1 (ILM.delete v fwd)
                bwd' = HashMap.insert a1 w bwd
                changed' = ILS.insert w changed
            in (w, Assoc fwd' bwd' equiv'' changed')
          -- v wins
          else
            let fwd' = ILM.delete w fwd
                changed' = ILS.insert w changed
            in (v, Assoc fwd' bwd equiv'' changed')

assocInsert :: (Coercible x Int, Ord x, Eq a, Hashable a) => x -> a -> State (Assoc x a) x
assocInsert x a = state (assocInsertInc x a)

assocFromList :: (Coercible x Int, Ord x, Eq a, Hashable a) => [(x, a)] -> Assoc x a
assocFromList = foldl' (\assoc (x, a) -> let (_, assoc') = assocInsertInc x a assoc in assoc') assocNew

assocToList :: Coercible x Int => Assoc x a -> [(x, a)]
assocToList = ILM.toList . assocFwd

-- | Forward lookup
assocLookupByKey :: (Coercible x Int) => x -> Assoc x a -> Maybe a
assocLookupByKey x (Assoc fwd _ equiv _) = ILM.lookup (efLookupRoot x equiv) fwd

-- -- | PARTIAL forward lookup
-- assocPartialLookupByKey :: (Coercible x Int) => x -> Assoc x a ->  a
-- assocPartialLookupByKey x = undefined

-- | Backward lookup
assocLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> Maybe x
assocLookupByValue a = HashMap.lookup a . assocBwd

-- -- | PARTIAL backward lookup
-- assocPartialLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> x
-- assocPartialLookupByValue a assoc = assocBwd assoc HashMap.! a

-- | Are there dead elements in the forward map from 'assocUpdate'?
assocCanCompact :: Assoc x a -> Bool
assocCanCompact = const False -- TODO XXX

assocCompactInc :: (Coercible x Int, Eq a, Hashable a) => Assoc x a -> (IntLikeMap x x, Assoc x a)
assocCompactInc assoc = (ILM.empty, assoc) -- TODO XXX

-- | Removes all dead elements from the forward map
assocCompact :: (Coercible x Int, Eq a, Hashable a) => State (Assoc x a) (IntLikeMap x x)
assocCompact = state assocCompactInc
