module Overeasy.Test.Assertions
  ( (===)
  , (/==)
  , assert
  , testGen
  , testUnit
  , MonadTest
  ) where

import Hedgehog (DiscardLimit, MonadTest, Property, PropertyT, ShrinkLimit, ShrinkRetries, TestLimit, assert, property,
                 withDiscards, withRetries, withShrinks, withTests, (/==), (===))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

unitProperty :: PropertyT IO () -> Property
unitProperty =
  withTests (1 :: TestLimit) .
  withDiscards (1 :: DiscardLimit) .
  withShrinks (0 :: ShrinkLimit) .
  withRetries (0 :: ShrinkRetries) .
  property

testUnit :: TestName -> PropertyT IO () -> TestTree
testUnit name = testProperty name . unitProperty

testGen :: TestName -> PropertyT IO () -> TestTree
testGen name = testProperty name . property
