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
  , assocLookupByKey
  , assocLookupByValue
  , assocDeleteByKey
  , assocDeleteByValue
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.Coerce (Coercible)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.Classes (Changed (..))
import Overeasy.IntLikeMap (IntLikeMap, deleteIntLikeMap, emptyIntLikeMap, insertIntLikeMap, lookupIntLikeMap,
                            sizeIntLikeMap)
import Overeasy.Source (Source, sourceAddInc, sourceNew, sourceSize)
import Overeasy.StateUtil (stateFail, stateFailChanged)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(IntLikeMap x a)
  , assocBwd :: !(HashMap a x)
  , assocSrc :: !(Source x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many elements are still in the map?
assocSize :: Assoc x a -> Int
assocSize = sizeIntLikeMap . assocFwd

-- | How many ids have ever been created?
assocTotalSize :: Assoc x a -> Int
assocTotalSize = sourceSize . assocSrc

-- | Creates a new 'Assoc' from a starting element
assocNew :: Coercible x Int => x -> Assoc x a
assocNew = Assoc emptyIntLikeMap HashMap.empty . sourceNew

-- private
assocAddInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAddInc a (Assoc fwd bwd src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in Just (n, Assoc (insertIntLikeMap n a fwd) (HashMap.insert a n bwd) src')
    Just _ -> Nothing

-- | Adds the given element to the 'Assoc' and returns a new id or 'Nothing' if it already exists
assocAdd :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Maybe x)
assocAdd = stateFail . assocAddInc

-- private
assocEnsureInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> ((Changed, x), Assoc x a)
assocEnsureInc a w@(Assoc fwd bwd src) =
  case HashMap.lookup a bwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in ((ChangedYes, n), Assoc (insertIntLikeMap n a fwd) (HashMap.insert a n bwd) src')
    Just x -> ((ChangedNo, x), w)

-- | Adds the given element to the 'Assoc' and returns a new id or the existing one on conflict
assocEnsure :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Changed, x)
assocEnsure = state . assocEnsureInc

-- | Lookup foward
assocLookupByKey :: (Coercible x Int) => x -> Assoc x a -> Maybe a
assocLookupByKey x = lookupIntLikeMap x . assocFwd

-- | Lookup backward
assocLookupByValue :: (Eq a, Hashable a) => a -> Assoc x a -> Maybe x
assocLookupByValue a = HashMap.lookup a . assocBwd

-- private
assocDeleteByKeyInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (Assoc x a)
assocDeleteByKeyInc a (Assoc fwd bwd n) = fmap (\x -> Assoc (deleteIntLikeMap x fwd) (HashMap.delete a bwd) n) (HashMap.lookup a bwd)

-- | Deletes an element by key
assocDeleteByKey :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) Changed
assocDeleteByKey = stateFailChanged . assocDeleteByKeyInc

-- private
assocDeleteByValueInc :: (Coercible x Int, Eq a, Hashable a) => x -> Assoc x a -> Maybe (Assoc x a)
assocDeleteByValueInc x (Assoc fwd bwd n) = fmap (\a -> Assoc (deleteIntLikeMap x fwd) (HashMap.delete a bwd) n) (lookupIntLikeMap x fwd)

-- | Deletes an element by value
assocDeleteByValue :: (Coercible x Int, Eq a, Hashable a) => x -> State (Assoc x a) Changed
assocDeleteByValue = stateFailChanged . assocDeleteByValueInc
