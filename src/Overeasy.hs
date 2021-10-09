{-# LANGUAGE DeriveAnyClass #-}

module Overeasy where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- class (Traversable p, Traversable f) => Unifiable p f | p -> f where
--   zipMatch :: p a -> f b -> Maybe (p (a, b))

-- data PatternF r f a =
--     PatternWildF
--   | PatternVarF !a
--   | PatternEmbedF !(f r)
--   deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
--   deriving anyclass (Hashable, NFData)

-- newtype Pattern f a = Pattern { unPattern :: PatternF (Pattern f a) f a }

-- newtype Match v f a = Match { unMatch :: PatternF v f a }

data ArithF a =
    ArithPlusF a a
  | ArithTimesF a a
  | ArithShiftLF a !Int
  | ArithShiftRF a !Int
  | ArithConstF !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

newtype Arith = Arith { unArith :: ArithF Arith }
  deriving newtype (Eq, Ord, Show, Hashable, NFData)
