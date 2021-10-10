{-# LANGUAGE DeriveAnyClass #-}

-- | An example of equality saturation using E-Graphs.
module Overeasy.Example where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data ArithF a =
    ArithPlusF a a
  | ArithTimesF a a
  | ArithShiftLF a !Int
  | ArithShiftRF a !Int
  | ArithConstF !Int
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

newtype Arith = Arith { unArith :: ArithF Arith }
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

type instance Base Arith = ArithF

instance Recursive Arith where
  project = unArith

instance Corecursive Arith where
  embed = Arith

-- TODO implement equality saturation!
