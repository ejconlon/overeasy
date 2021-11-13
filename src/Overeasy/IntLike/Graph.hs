module Overeasy.IntLike.Graph
  ( IntLikeGraph (..)
  , adjacencyIntMultiMap
  , vertexList
  , fromDirectedEdges
  , fromUndirectedEdges
  , reachable
  , Component (..)
  , undirectedComponents
  ) where

import Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap as AdjacencyIntMap
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm as AIMA
import Algebra.Graph.Class (Graph (..))
import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Tuple (swap)
import Overeasy.IntLike.Equiv (IntLikeEquiv)
import qualified Overeasy.IntLike.Equiv as ILE
import Overeasy.IntLike.Map (IntLikeMap (..))
import Overeasy.IntLike.MultiMap (IntLikeMultiMap)
import Overeasy.IntLike.Set (IntLikeSet (..))
import qualified Overeasy.IntLike.Set as ILS

newtype IntLikeGraph x = IntLikeGraph { unIntLikeGraph :: AdjacencyIntMap }
  deriving newtype (Eq, Show, NFData)

instance Coercible x Int => Graph (IntLikeGraph x) where
  type Vertex (IntLikeGraph x) = x
  empty = IntLikeGraph AdjacencyIntMap.empty
  vertex v = IntLikeGraph (AdjacencyIntMap.vertex (coerce v))
  overlay x y = IntLikeGraph (AdjacencyIntMap.overlay (unIntLikeGraph x) (unIntLikeGraph y))
  connect x y = IntLikeGraph (AdjacencyIntMap.connect (unIntLikeGraph x) (unIntLikeGraph y))

adjacencyIntMultiMap :: IntLikeGraph x -> IntLikeMultiMap x x
adjacencyIntMultiMap = coerce . AdjacencyIntMap.adjacencyIntMap . unIntLikeGraph
{-# INLINE adjacencyIntMultiMap #-}

vertexList :: Coercible x Int => IntLikeGraph x -> [x]
vertexList = coerce . AdjacencyIntMap.vertexList . unIntLikeGraph
{-# INLINE vertexList #-}

fromDirectedEdges :: Coercible x Int => [(x, x)] -> IntLikeGraph x
fromDirectedEdges = IntLikeGraph . AdjacencyIntMap.edges . coerce
{-# INLINE fromDirectedEdges #-}

fromUndirectedEdges :: Coercible x Int => [(x, x)] -> IntLikeGraph x
fromUndirectedEdges es = overlay (fromDirectedEdges es) (fromDirectedEdges (fmap swap es))
{-# INLINE fromUndirectedEdges #-}

reachable :: Coercible x Int => x -> IntLikeGraph x -> [x]
reachable x = coerce . AIMA.reachable (coerce x) . unIntLikeGraph
{-# INLINE reachable #-}

newtype Component = Component { unComponent :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Hashable, NFData)

undirectedComponents :: Coercible x Int => [(x, x)] -> IntLikeEquiv Component x
undirectedComponents es = go 0 startVs ILE.empty where
  g = fromUndirectedEdges es
  startVs = ILS.fromList (vertexList g)
  go i vs eqv =
    case ILS.minView vs of
      Nothing -> eqv
      Just (v, vs') ->
        let rs = reachable v g
            -- partial: ok by construction of graph and defn of reachable
            eqv' = foldl' (flip (ILE.partialInsert (Component i))) eqv rs
            vs'' = foldl' (flip ILS.delete) vs' rs
        in go (i + 1) vs'' eqv'
