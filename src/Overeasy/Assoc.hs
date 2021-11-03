{-# LANGUAGE DeriveAnyClass #-}

-- | Associates elements with unique ids drawn from a 'Source'
module Overeasy.Assoc
  ( Assoc
  , assocSize
  , assocNew
  , assocAdd
  , assocEnsure
  , assocFwd
  , assocBwd
  , assocDeadFwd
  , assocDeadBwd
  , assocLookupByKey
  , assocPartialLookupByKey
  , assocLookupByValue
  , assocPartialLookupByValue
  , assocDeleteByKey
  , assocDeleteByValue
  , assocUpdate
  , assocNeedsClean
  , assocClean
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.State.Strict (MonadState (..), State)
import Data.Coerce (Coercible)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Source (Source, sourceAddInc, sourceNew, sourceSize, sourceSkipInc)
import Overeasy.StateUtil (stateFail, stateFailChanged)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(IntLikeMap x a)
  , assocBwd :: !(HashMap a x)
  , assocDeadFwd :: !(IntLikeSet x)
  , assocDeadBwd :: !(HashSet a)
  , assocSrc :: !(Source x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many elements are still in the map?
assocSize :: Assoc x a -> Int
assocSize = ILM.size . assocFwd

-- | How many ids have ever been created?
assocTotalSize :: Assoc x a -> Int
assocTotalSize = sourceSize . assocSrc

-- | Creates a new 'Assoc' from a starting element
assocNew :: Coercible x Int => x -> Assoc x a
assocNew = Assoc ILM.empty HashMap.empty ILS.empty HashSet.empty . sourceNew

-- private
assocAddInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAddInc a (Assoc fwd bwd deadFwd deadBwd src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in Just (n, Assoc (ILM.insert n a fwd) (HashMap.insert a n bwd) deadFwd deadBwd src')
    Just _ -> Nothing

-- | Adds the given element to the 'Assoc' and returns a new id or 'Nothing' if it already exists
assocAdd :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Maybe x)
assocAdd = stateFail . assocAddInc

-- private
assocEnsureInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> ((Changed, x), Assoc x a)
assocEnsureInc a w@(Assoc fwd bwd deadFwd deadBwd src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in ((ChangedYes, n), Assoc (ILM.insert n a fwd) (HashMap.insert a n bwd) deadFwd deadBwd src')
    Just x -> ((ChangedNo, x), w)

-- | Adds the given element to the 'Assoc' and returns a new id or the existing one on conflict
assocEnsure :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Changed, x)
assocEnsure = state . assocEnsureInc

-- | Forward lookup
assocLookupByKey :: (Coercible x Int) => x -> Assoc x a -> Maybe a
assocLookupByKey x = ILM.lookup x . assocFwd

-- | PARTIAL forward lookup
assocPartialLookupByKey :: (Coercible x Int) => x -> Assoc x a ->  a
assocPartialLookupByKey x = ILM.partialLookup x . assocFwd

-- | Backward lookup
assocLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> Maybe x
assocLookupByValue a = HashMap.lookup a . assocBwd

-- | PARTIAL backward lookup
assocPartialLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> x
assocPartialLookupByValue a assoc = assocBwd assoc HashMap.! a

-- private
assocDeleteByKeyInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (Assoc x a)
assocDeleteByKeyInc a (Assoc fwd bwd deadFwd deadBwd n) = fmap (\x -> Assoc (ILM.delete x fwd) (HashMap.delete a bwd) (ILS.delete x deadFwd) (HashSet.delete a deadBwd) n) (HashMap.lookup a bwd)

-- | Deletes an element by key
assocDeleteByKey :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) Changed
assocDeleteByKey = stateFailChanged . assocDeleteByKeyInc

-- private
assocDeleteByValueInc :: (Coercible x Int, Eq a, Hashable a) => x -> Assoc x a -> Maybe (Assoc x a)
assocDeleteByValueInc x (Assoc fwd bwd deadFwd deadBwd n) = fmap (\a -> Assoc (ILM.delete x fwd) (HashMap.delete a bwd) (ILS.delete x deadFwd) (HashSet.delete a deadBwd) n) (ILM.lookup x fwd)

-- | Deletes an element by value
assocDeleteByValue :: (Coercible x Int, Eq a, Hashable a) => x -> State (Assoc x a) Changed
assocDeleteByValue = stateFailChanged . assocDeleteByValueInc

-- | Updates the assoc. You may need to clean the 'Assoc' with 'assocClean' when you have finished updating.
-- If (x, a0) is in the assoc fwd and you update with (x, a1) where a0 /= a1, you must never call with a0 again.
-- May break the 1-1 fwd and bwd mappings until you clean.
assocUpdate :: (Coercible x Int, Eq x, Eq a, Hashable a) => x -> a -> State (Assoc x a) x
assocUpdate x a1 = state $ \assoc@(Assoc fwd bwd deadFwd deadBwd n) ->
  case HashMap.lookup a1 bwd of
    Just y ->
      if x == y
        -- We are inserting the exact pair that exists - This is a no-op.
        then (x, assoc)
        -- If a is already in the map (to y), update fwd x a, mark x for deletion, then return y
        -- This makes fwd many-to-one until clean. No elem in bwd will point to x.
        else
          let fwd' = ILM.insert x a1 fwd
              deadFwd' = ILS.insert x deadFwd
          in (y, Assoc fwd' bwd deadFwd' deadBwd n)
    Nothing ->
      case ILM.lookup x fwd of
        -- x was never in the map - simply insert
        Nothing ->
          let fwd' = ILM.insert x a1 fwd
              bwd' = HashMap.insert a1 x bwd
              n' = sourceSkipInc x n
          in (x, Assoc fwd' bwd' deadFwd deadBwd n')
        -- Update fwd bwd for x a, mark original a for deletion, then return x
        -- This makes bwd many-to-one.
        Just a0 ->
          let fwd' = ILM.insert x a1 fwd
              bwd' = HashMap.insert a1 x bwd
              deadBwd' = HashSet.insert a0 deadBwd
          in (x, Assoc fwd' bwd' deadFwd deadBwd' n)

-- | Are there dead elements in the forward map from 'assocUpdate'?
assocNeedsClean :: Assoc x a -> Bool
assocNeedsClean assoc = not (ILS.null (assocDeadFwd assoc) && HashSet.null (assocDeadBwd assoc))

-- | Removes all dead elements from the forward map
assocClean :: (Coercible x Int, Eq a, Hashable a) => State (Assoc x a) ()
assocClean = do
  assoc@(Assoc fwd bwd deadFwd deadBwd n) <- get
  when (assocNeedsClean assoc) $ do
    let fwd' = foldr ILM.delete fwd (ILS.toList deadFwd)
    let bwd' = foldr HashMap.delete bwd deadBwd
    put (Assoc fwd' bwd' ILS.empty HashSet.empty n)
