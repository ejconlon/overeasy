{-# LANGUAGE DeriveAnyClass #-}

-- | A source of unique ids
module Overeasy.Source
  ( Source
  , sourceSize
  , sourceNew
  , sourceAddInc
  , sourceAdd
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import GHC.Generics (Generic)

-- private ctor
data Source x = Source
  { sourceSize :: !Int  -- ^ How many ids have ever been created?
  , sourceNextId :: !x
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Creates a new 'Source' from a starting id
sourceNew :: x -> Source x
sourceNew = Source 0

-- | Generates the next id from the source (purely)
sourceAddInc :: Enum x => Source x -> (x, Source x)
sourceAddInc (Source s x) = (x, Source (succ s) (succ x))

-- | Generates the next id from the source (statefully)
sourceAdd :: Enum x => State (Source x) x
sourceAdd = state sourceAddInc
