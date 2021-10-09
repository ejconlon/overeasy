{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.EGraph
  ( EGraph
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.Source
import Overeasy.UnionFind

-- private ctor
newtype EClassId x = EClassId { unEClassId :: Int } deriving newtype (Enum, Eq, Ord, Show, Hashable, NFData)

data EGraph x a = EGraph
  { egSource :: Source x
  , egClasses :: UnionFind x
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)
