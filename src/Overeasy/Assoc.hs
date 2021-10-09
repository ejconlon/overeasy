{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Assoc
  ( Assoc
  , assocSize
  , assocNew
  , assocAdd
  , assocFwd
  , assocBwd
  ) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

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

assocAdd :: (Enum x, Eq x, Hashable x, Eq a, Hashable a) => a -> Assoc x a -> Maybe (x, Assoc x a)
assocAdd a (Assoc fwd bwd n) =
  case HashMap.lookup a fwd of
    Nothing -> Just (n, Assoc (HashMap.insert a n fwd) (HashMap.insert n a bwd) (succ n))
    Just _ -> Nothing
