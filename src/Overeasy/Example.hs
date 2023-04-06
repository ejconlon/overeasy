{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | An example using arithmetic expressions.
module Overeasy.Example
  ( Arith (..)
  , ArithF (..)
  , exampleGraph
  , examplePat
  , exampleMain
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (execState)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Overeasy.EGraph (EClassId (..), EGraph, egAddTerm, egMerge, egNew, noAnalysis)
import Overeasy.Matching (Pat, match)
import Unfree (pattern FreeEmbed, pattern FreePure)

-- | Arithmetic expressions.
-- 'ArithF' is the base functor for this type.
data Arith
  = ArithPlus Arith Arith
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

-- | Creates a simple e-graph with the equality `2 + 2 = 4`.
exampleGraph :: EGraph () ArithF
exampleGraph = flip execState egNew $ do
  -- We don't need to analyze here.
  let ana = noAnalysis
  -- Some simple terms:
  let termFour = ArithConst 4
      termTwo = ArithConst 2
      termPlus = ArithPlus termTwo termTwo
  -- Add the term `4`
  (_, cidFour) <- egAddTerm ana termFour
  -- Add the term `2`
  (_, _cidTwo) <- egAddTerm ana termTwo
  -- Add the term `2 + 2`
  (_, cidPlus) <- egAddTerm ana termPlus
  -- Merge `4` and `2 + 2`
  _ <- egMerge cidFour cidPlus
  pure ()

-- | Creates a simple pattern to match nodes like `x + x`.
examplePat :: Pat ArithF String
examplePat = FreeEmbed (ArithPlusF (FreePure "x") (FreePure "x"))

-- | Build an e-graph, e-match on it, and print the result.
exampleMain :: IO ()
exampleMain = do
  let eg = exampleGraph
  putStrLn "e-graph:"
  print eg
  let pat = examplePat
  putStrLn "pattern:"
  print pat
  let results = match pat eg
  putStrLn "e-matches:"
  print results
