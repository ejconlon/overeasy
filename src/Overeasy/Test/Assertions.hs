module Overeasy.Test.Assertions
  ( MonadAssert (..)
  , Unit (..)
  , (@?)
  , (@?=)
  , (@/=)
  , assertUnaryPredicate
  , assertBinaryPredicate
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack)
import Hedgehog (PropertyT)
import qualified Hedgehog as HG
import qualified Test.Tasty.HUnit as HU
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans (lift)

assertUnaryPredicate :: (MonadFail m, Show a) => (a -> Bool) -> String -> a -> m ()
assertUnaryPredicate predicate preface value = unless (predicate value) (fail msg) where
  msg = (if null preface then "" else preface ++ "\n") ++ "value: " ++ show value

assertBinaryPredicate :: (MonadFail m, Show a) => (a -> a -> Bool) -> String -> a -> a -> m ()
assertBinaryPredicate predicate preface left right = unless (predicate left right) (fail msg) where
  msg = (if null preface then "" else preface ++ "\n") ++ "left: " ++ show left ++ "\n right: " ++ show right

class MonadFail m => MonadAssert m where
  assertTrue :: HasCallStack => Bool -> String -> m ()
  assertTrue a p = assertUnaryPredicate id p a
  assertEqual :: (HasCallStack, Eq a, Show a) => a -> a -> m ()
  assertEqual = assertBinaryPredicate (==) "Expected equal"
  assertNotEqual :: (HasCallStack, Eq a, Show a) => a -> a -> m ()
  assertNotEqual = assertBinaryPredicate (/=) "Expected not equal"

infix 1 @?, @?=, @/=

(@?) :: (MonadAssert m, HasCallStack) => Bool -> String -> m ()
(@?) = assertTrue

(@?=) :: (MonadAssert m, HasCallStack, Eq a, Show a) => a -> a -> m ()
(@?=) = assertEqual

(@/=) :: (MonadAssert m, HasCallStack, Eq a, Show a) => a -> a -> m ()
(@/=) = assertNotEqual

instance Monad m => MonadAssert (PropertyT m) where
  assertTrue x p = HG.footnote p *> HG.assert x
  assertEqual = (HG.===)
  assertNotEqual = (HG./==)

instance MonadAssert m => MonadAssert (StateT s m) where
  assertTrue x p = lift (assertTrue x p)
  assertEqual x y = lift (assertEqual x y)
  assertNotEqual x y = lift (assertNotEqual x y)

newtype Unit a = Unit { runUnit :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadFail Unit where
  fail = Unit . HU.assertFailure

instance MonadAssert Unit where
  assertTrue x p = Unit (x HU.@? p)
  assertEqual x y = Unit (x HU.@?= y)
