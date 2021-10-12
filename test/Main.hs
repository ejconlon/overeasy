module Main (main) where

import Control.Monad.State (State, runState)
import Data.Functor (($>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testS :: s -> State s a -> ((a, s) -> IO ()) -> IO s
testS s act check =
  let p@(_, s') = runState act s
  in check p $> s'

testSimple :: TestTree
testSimple = testCase "simple" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual @?= expected

main :: IO ()
main = defaultMain (testGroup "Overeasy" [testSimple])
