{-# LANGUAGE DeriveAnyClass #-}

module Overeasy.Source
  ( Source
  , sourceSize
  , sourceNew
  , sourceAdd
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- private ctor
data Source x = Source
  { sourceSize :: !Int
  , sourceNextId :: !x
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

sourceNew :: x -> Source x
sourceNew = Source 0

sourceAdd :: Enum x => Source x -> (x, Source x)
sourceAdd (Source s x) = (x, Source (succ s) (succ x))
