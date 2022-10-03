{-# LANGUAGE DeriveAnyClass #-}

-- See 'Source'.
module Overeasy.Source
  ( Source
  , sourceSize
  , sourceNew
  , sourceAddInc
  , sourceAdd
  , sourceSkipInc
  , sourceSkip
  , sourcePeek
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, modify', state)
import Data.Coerce (Coercible, coerce)
import GHC.Generics (Generic)

-- | A source of unique ids
data Source x = Source
  { sourceSize :: !Int
  -- ^ How many ids have ever been created?
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

-- | Skips past the given id (purely)
sourceSkipInc :: Coercible x Int => x -> Source x -> Source x
sourceSkipInc y (Source s x) =
  let z = coerce y
  in Source (s + 1) (max x (z + 1))

-- | Skips past the given id (statefully)
sourceSkip :: Coercible x Int => x -> State (Source x) ()
sourceSkip = modify' . sourceSkipInc

-- | Peek at the next id
sourcePeek :: Coercible x Int => Source x -> x
sourcePeek = coerce . sourceNextId
