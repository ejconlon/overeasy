module Overeasy.IntLikeGraph
  ( IntLikeGraph (..)
  ) where

import Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap as AdjacencyIntMap
import Algebra.Graph.Class (Graph (..))
import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)

newtype IntLikeGraph x = IntLikeGraph { unIntLikeGraph :: AdjacencyIntMap }
  deriving newtype (Eq, Show, NFData)

instance Coercible x Int => Graph (IntLikeGraph x) where
  type Vertex (IntLikeGraph x) = x
  empty = IntLikeGraph AdjacencyIntMap.empty
  vertex v = IntLikeGraph (AdjacencyIntMap.vertex (coerce v))
  overlay x y = IntLikeGraph (AdjacencyIntMap.overlay (unIntLikeGraph x) (unIntLikeGraph y))
  connect x y = IntLikeGraph (AdjacencyIntMap.connect (unIntLikeGraph x) (unIntLikeGraph y))
