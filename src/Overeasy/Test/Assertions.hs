module Overeasy.Test.Assertions
  ( (===)
  , (/==)
  , assert
  , testGen
  , testUnit
  , setupTests
  , MonadTest
  , TestLimit
  ) where

import Control.Monad (when)
import Hedgehog (DiscardLimit, MonadTest, Property, PropertyT, ShrinkLimit, ShrinkRetries, TestLimit, assert, property,
                 withDiscards, withRetries, withShrinks, withTests, (/==), (===))
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

limProperty :: TestLimit -> PropertyT IO () -> Property
limProperty lim =
  withTests lim .
  withDiscards (1 :: DiscardLimit) .
  withShrinks (0 :: ShrinkLimit) .
  withRetries (0 :: ShrinkRetries) .
  property

unitProperty :: PropertyT IO () -> Property
unitProperty = limProperty (1 :: TestLimit)

testUnit :: TestName -> PropertyT IO () -> TestTree
testUnit name = testProperty name . unitProperty

testGen :: TestName -> TestLimit -> PropertyT IO () -> TestTree
testGen name lim = testProperty name . withTests lim . property

setupTests :: IO TestLimit
setupTests = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  mayLimStr <- lookupEnv "GENLIMIT"
  pure (maybe 100 (fromInteger . read) mayLimStr)
