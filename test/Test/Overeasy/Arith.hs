{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Overeasy.Arith
  ( ArithF (..)
  , Arith (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Arith =
    ArithPlus Arith Arith
  | ArithTimes Arith Arith
  | ArithShiftL Arith !Int
  | ArithShiftR Arith !Int
  | ArithConst !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- Generates 'ArithF' and other recursion-schemes boilerplate
makeBaseFunctor ''Arith

deriving stock instance Eq a => Eq (ArithF a)
deriving stock instance Show a => Show (ArithF a)
deriving stock instance Generic (ArithF a)
deriving anyclass instance Hashable a => Hashable (ArithF a)
deriving anyclass instance NFData a => NFData (ArithF a)
