{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.EGraph
  ( EGraph
  , egNew
  ) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)
import Overeasy.Source (Source, sourceNew)
import Overeasy.UnionFind (UnionFind, ufNew)

-- private ctor
data EGraph x f a = EGraph
  { egSource :: Source x
  , egUnionFind :: UnionFind x
  , egClassMap :: HashMap x a
  , egHashCons :: HashMap (f x) x
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

egNew :: x -> EGraph x f a
egNew x = EGraph (sourceNew x) ufNew HashMap.empty HashMap.empty
