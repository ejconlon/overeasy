{-# LANGUAGE DeriveAnyClass #-}

module Overeasy where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- class (Traversable p, Traversable f) => Unifiable p f | p -> f where
--   zipMatch :: p a -> f b -> Maybe (p (a, b))

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
