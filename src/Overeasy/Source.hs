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
import Data.Coerce (Coercible, coerce)
import GHC.Generics (Generic)

-- private ctor
data Source x = Source
  { sourceSize :: !Int  -- ^ How many ids have ever been created?
  , sourceNextId :: !Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Creates a new 'Source' from a starting id
sourceNew :: Coercible x Int => x -> Source x
sourceNew = Source 0 . coerce

-- | Generates the next id from the source (purely)
sourceAddInc :: Coercible x Int => Source x -> (x, Source x)
sourceAddInc (Source s x) = (coerce x, Source (s + 1) (x + 1))

-- | Generates the next id from the source (statefully)
sourceAdd :: Coercible x Int => State (Source x) x
sourceAdd = state sourceAddInc
