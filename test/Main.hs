module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testSimple :: TestTree
testSimple = testCase "simple" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual @?= expected

main :: IO ()
main = defaultMain (testGroup "Overeasy" [testSimple])
