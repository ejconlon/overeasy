{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Assoc
  ( Assoc
  , assocSize
  , assocNew
  , assocAdd
  , AssocEnsureResult (..)
  , assocEnsure
  , assocFwd
  , assocBwd
  -- , assocRemoveByKey
  -- , assocRemoveByValue
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.StateUtil (stateFail, stateFailBool)

-- private ctor
data Assoc x a = Assoc
  { assocFwd :: !(HashMap a x)
  , assocBwd :: !(HashMap x a)
  , assocNextId :: !x
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

assocSize :: Assoc x a -> Int
assocSize = HashMap.size . assocFwd

assocNew :: x -> Assoc x a
assocNew = Assoc HashMap.empty HashMap.empty

-- private
assocAddInc :: (Enum x, Eq x, Hashable x, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAddInc a (Assoc fwd bwd n) =
  case HashMap.lookup a fwd of
    Nothing -> Just (n, Assoc (HashMap.insert a n fwd) (HashMap.insert n a bwd) (succ n))
    Just _ -> Nothing

assocAdd :: (Enum x, Eq x, Hashable x, Eq a, Hashable a) => a -> State (Assoc x a) (Maybe x)
assocAdd = stateFail . assocAddInc

data AssocEnsureResult = AssocEnsureAdded | AssocEnsureExists
  deriving stock (Eq, Show)

-- private
assocEnsureInc :: (Enum x, Eq x, Hashable x, Eq a, Hashable a) => a -> Assoc x a -> ((x, AssocEnsureResult), Assoc x a)
assocEnsureInc a w@(Assoc fwd bwd n) =
  case HashMap.lookup a fwd of
    Nothing -> ((n, AssocEnsureAdded), Assoc (HashMap.insert a n fwd) (HashMap.insert n a bwd) (succ n))
    Just x -> ((x, AssocEnsureExists), w)

assocEnsure :: (Enum x, Eq x, Hashable x, Eq a, Hashable a) => a -> State (Assoc x a) (x, AssocEnsureResult)
assocEnsure = state . assocEnsureInc

-- private
assocRemoveByKeyInc :: (Eq x, Hashable x, Eq a, Hashable a) => a -> Assoc x a -> Maybe (Assoc x a)
assocRemoveByKeyInc a (Assoc fwd bwd n) = fmap (\x -> Assoc (HashMap.delete a fwd) (HashMap.delete x bwd) n) (HashMap.lookup a fwd)

assocRemoveByKey :: (Eq x, Hashable x, Eq a, Hashable a) => a -> State (Assoc x a) Bool
assocRemoveByKey = stateFailBool . assocRemoveByKeyInc

-- private
assocRemoveByValueInc :: (Eq x, Hashable x, Eq a, Hashable a) => x -> Assoc x a -> Maybe (Assoc x a)
assocRemoveByValueInc x (Assoc fwd bwd n) = fmap (\a -> Assoc (HashMap.delete a fwd) (HashMap.delete x bwd) n) (HashMap.lookup x bwd)

assocRemoveByValue :: (Eq x, Hashable x, Eq a, Hashable a) => x -> State (Assoc x a) Bool
assocRemoveByValue = stateFailBool . assocRemoveByValueInc
