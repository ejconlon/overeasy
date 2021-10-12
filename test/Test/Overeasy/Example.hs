{-# LANGUAGE DeriveAnyClass #-}

module Test.Overeasy.Example
  ( ArithF (..)
  , Arith (..)
  , pattern ArithPlus
  , pattern ArithTimes
  , pattern ArithShiftL
  , pattern ArithShiftR
  , pattern ArithConst
  ) where

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

pattern ArithPlus :: Arith -> Arith -> Arith
pattern ArithPlus a b = Arith (ArithPlusF a b)

pattern ArithTimes :: Arith -> Arith -> Arith
pattern ArithTimes a b = Arith (ArithTimesF a b)

pattern ArithShiftL :: Arith -> Int -> Arith
pattern ArithShiftL a b = Arith (ArithShiftLF a b)

pattern ArithShiftR :: Arith -> Int -> Arith
pattern ArithShiftR a b = Arith (ArithShiftRF a b)

pattern ArithConst :: Int -> Arith
pattern ArithConst a = Arith (ArithConstF a)

{-# COMPLETE ArithPlus, ArithTimes, ArithShiftL, ArithShiftR, ArithConst #-}

type instance Base Arith = ArithF

instance Recursive Arith where
  project = unArith

instance Corecursive Arith where
  embed = Arith
