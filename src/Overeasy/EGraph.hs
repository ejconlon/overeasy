{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.EGraph
  ( EGraph
  , egNew
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState (..), State, modify', runState, state)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)
import Overeasy.Source (Source, sourceNew)
import Overeasy.UnionFind (UnionFind, ufFind, ufFindState, ufNew)

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

-- private
egUfState :: State (UnionFind x) b -> State (EGraph x f a) b
egUfState act = state (\eg -> fmap (\u' -> eg { egUnionFind = u' }) (runState act (egUnionFind eg)))

egCanonicalize :: (Eq x, Hashable x, Traversable f) => f x -> EGraph x f a -> Maybe (f x)
egCanonicalize fx eg = traverse (`ufFind` egUnionFind eg) fx

-- egCanonicalizeInc :: (Eq x, Hashable x, Traversable f) => f x -> EGraph x f a -> Maybe (f x, EGraph x f a)
-- egCanonicalizeInc fx eg =
--   let u = egUnionFind eg
--       (mfx', u') = runState (traverse ufFindState fx) u
--   in fmap (, eg { egUnionFind = u' }) mfx'

-- egCanonicalizeState :: (Eq x, Hashable x, Traversable f) => f x -> State (EGraph x f a) (Maybe (f x))
-- egCanonicalizeState = egUfState . traverse ufFindState
