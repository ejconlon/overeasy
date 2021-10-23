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
  , assocDead
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
import Control.Monad (unless)
import Control.Monad.State.Strict (MonadState (..), State, gets, modify')
import Data.Coerce (Coercible)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLikeMap (IntLikeMap)
import qualified Overeasy.IntLikeMap as ILM
import Overeasy.IntLikeSet (IntLikeSet)
import qualified Overeasy.IntLikeSet as ILS
import Overeasy.Source (Source, sourceAddInc, sourceNew, sourceSize)
import Overeasy.StateUtil (stateFail, stateFailChanged)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(IntLikeMap x a)
  , assocBwd :: !(HashMap a x)
  , assocDead :: !(IntLikeSet x)
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
assocNew = Assoc ILM.empty HashMap.empty ILS.empty . sourceNew

-- private
assocAddInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAddInc a (Assoc fwd bwd dead src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in Just (n, Assoc (ILM.insert n a fwd) (HashMap.insert a n bwd) dead src')
    Just _ -> Nothing

-- | Adds the given element to the 'Assoc' and returns a new id or 'Nothing' if it already exists
assocAdd :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Maybe x)
assocAdd = stateFail . assocAddInc

-- private
assocEnsureInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> ((Changed, x), Assoc x a)
assocEnsureInc a w@(Assoc fwd bwd dead src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in ((ChangedYes, n), Assoc (ILM.insert n a fwd) (HashMap.insert a n bwd) dead src')
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
assocDeleteByKeyInc a (Assoc fwd bwd dead n) = fmap (\x -> Assoc (ILM.delete x fwd) (HashMap.delete a bwd) (ILS.delete x dead) n) (HashMap.lookup a bwd)

-- | Deletes an element by key
assocDeleteByKey :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) Changed
assocDeleteByKey = stateFailChanged . assocDeleteByKeyInc

-- private
assocDeleteByValueInc :: (Coercible x Int, Eq a, Hashable a) => x -> Assoc x a -> Maybe (Assoc x a)
assocDeleteByValueInc x (Assoc fwd bwd dead n) = fmap (\a -> Assoc (ILM.delete x fwd) (HashMap.delete a bwd) (ILS.delete x dead) n) (ILM.lookup x fwd)

-- | Deletes an element by value
assocDeleteByValue :: (Coercible x Int, Eq a, Hashable a) => x -> State (Assoc x a) Changed
assocDeleteByValue = stateFailChanged . assocDeleteByValueInc

-- | Updates the assoc, returning the best key on conflict.
-- You may need to clean the 'Assoc' with 'assocClean' when you have finished updating.
assocUpdate :: (Coercible x Int, Eq x, Eq a, Hashable a) => (x -> x -> x) -> x -> a -> State (Assoc x a) x
assocUpdate onConflict x a = do
  m <- gets (assocLookupByValue a)
  case m of
    Nothing -> do
      modify' $ \(Assoc fwd bwd dead n) ->
        let fwd' = ILM.insert x a fwd
            bwd' = HashMap.insert a x bwd
        in Assoc fwd' bwd' dead n
      pure x
    Just y -> do
      let toKeep = onConflict x y
          toDelete = if x == toKeep then y else x
      modify' $ \(Assoc fwd bwd dead n) ->
        let fwd' = ILM.insert toDelete a fwd
            bwd' = HashMap.insert a toKeep bwd
            dead' = ILS.insert toDelete dead
        in Assoc fwd' bwd' dead' n
      pure toKeep

-- | Are there dead elements in the forward map from 'assocUpdate'?
assocNeedsClean :: Assoc x a -> Bool
assocNeedsClean = not . ILS.null . assocDead

-- | Removes all dead elements from the forward map
assocClean :: Coercible x Int => State (Assoc x a) ()
assocClean = do
  Assoc fwd bwd dead n <- get
  unless (ILS.null dead) $ do
    let fwd' = foldr ILM.delete fwd (ILS.toList dead)
    put (Assoc fwd' bwd ILS.empty n)
