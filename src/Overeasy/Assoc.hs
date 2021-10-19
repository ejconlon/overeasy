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
  , assocRemoveByKey
  , assocRemoveByValue
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
  { assocFwd :: !(HashMap a x)
  , assocBwd :: !(IntLikeMap x a)
  , assocSrc :: !(Source x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | How many elements are still in the map?
assocSize :: Assoc x a -> Int
assocSize = sizeIntLikeMap . assocBwd

-- | How many ids have ever been created?
assocTotalSize :: Assoc x a -> Int
assocTotalSize = sourceSize . assocSrc

-- | Creates a new 'Assoc' from a starting element
assocNew :: Coercible x Int => x -> Assoc x a
assocNew = Assoc HashMap.empty emptyIntLikeMap . sourceNew

-- private
assocAddInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAddInc a (Assoc fwd bwd src) =
  case HashMap.lookup a fwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in Just (n, Assoc (HashMap.insert a n fwd) (insertIntLikeMap n a bwd) src')
    Just _ -> Nothing

-- | Adds the given element to the 'Assoc' and returns a new id or 'Nothing' if it already exists
assocAdd :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Maybe x)
assocAdd = stateFail . assocAddInc

-- private
assocEnsureInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> ((Changed, x), Assoc x a)
assocEnsureInc a w@(Assoc fwd bwd src) =
  case HashMap.lookup a fwd of
    Nothing ->
      let (n, src') = sourceAddInc src
      in ((ChangedYes, n), Assoc (HashMap.insert a n fwd) (insertIntLikeMap n a bwd) src')
    Just x -> ((ChangedNo, x), w)

-- | Adds the given element to the 'Assoc' and returns a new id or the existing one on conflict
assocEnsure :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) (Changed, x)
assocEnsure = state . assocEnsureInc

-- private
assocRemoveByKeyInc :: (Coercible x Int, Eq a, Hashable a) => a -> Assoc x a -> Maybe (Assoc x a)
assocRemoveByKeyInc a (Assoc fwd bwd n) = fmap (\x -> Assoc (HashMap.delete a fwd) (deleteIntLikeMap x bwd) n) (HashMap.lookup a fwd)

-- | Removes an element by key
assocRemoveByKey :: (Coercible x Int, Eq a, Hashable a) => a -> State (Assoc x a) Changed
assocRemoveByKey = stateFailChanged . assocRemoveByKeyInc

-- private
assocRemoveByValueInc :: (Coercible x Int, Eq a, Hashable a) => x -> Assoc x a -> Maybe (Assoc x a)
assocRemoveByValueInc x (Assoc fwd bwd n) = fmap (\a -> Assoc (HashMap.delete a fwd) (deleteIntLikeMap x bwd) n) (lookupIntLikeMap x bwd)

-- | Removes an element by value
assocRemoveByValue :: (Coercible x Int, Eq a, Hashable a) => x -> State (Assoc x a) Changed
assocRemoveByValue = stateFailChanged . assocRemoveByValueInc
