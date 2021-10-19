module Main (main) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalStateT, runState)
import Data.Char (chr, ord)
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisOff (..), EGraph, egAddTerm, egClassSize, egMerge, egNeedsRebuild, egNew, egNodeSize,
                        egTotalClassSize)
import Overeasy.IntLikeMap (fromListIntLikeMap)
import Overeasy.IntLikeSet (IntLikeSet, emptyIntLikeSet, fromListIntLikeSet)
import Overeasy.UnionFind (MergeRes (..), UnionFind (..), ufAdd, ufMembers, ufMerge, ufNew, ufRoots, ufTotalSize)
import Test.Overeasy.Assertions ((@/=))
import Test.Overeasy.Example (ArithF, pattern ArithConst, pattern ArithPlus)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

applyS :: State s a -> StateT s IO a
applyS = state . runState

applyS_ :: State s a -> StateT s IO ()
applyS_ = void . applyS

testS :: (s -> IO a) -> StateT s IO a
testS p = get >>= liftIO . p

applyTestS :: State s a -> (a -> s -> IO b) -> StateT s IO b
applyTestS act check = do
  a <- applyS act
  s <- get
  liftIO (check a s)

runS :: s -> StateT s IO () -> IO ()
runS = flip evalStateT

newtype V = V { unV :: Int }
  deriving newtype (Eq, Ord)

instance Show V where
  show = show . fromV

toV :: Char -> V
toV = V . ord

fromV :: V -> Char
fromV = chr . unV

setV :: String -> IntLikeSet V
setV = fromListIntLikeSet . fmap toV

type UF = UnionFind V

runUF :: StateT UF IO () -> IO ()
runUF = runS ufNew

testUfSimple :: TestTree
testUfSimple = testCase "UF simple" $ runUF $ do
  testS $ \uf -> ufSize uf @?= 0
  testS $ \uf -> ufTotalSize uf @?= 0
  applyTestS ufRoots $ \rs _ -> rs @?= emptyIntLikeSet
  applyS (ufAdd (toV 'a'))
  testS $ \uf -> ufSize uf @?= 1
  testS $ \uf -> ufTotalSize uf @?= 1
  applyTestS ufRoots $ \rs _ -> rs @?= setV "a"
  applyS (ufAdd (toV 'b'))
  applyS (ufAdd (toV 'c'))
  testS $ \uf -> ufSize uf @?= 3
  testS $ \uf -> ufTotalSize uf @?= 3
  applyTestS ufRoots $ \rs _ -> rs @?= setV "abc"
  applyTestS (ufMerge (toV 'a') (toV 'c')) $ \res uf -> do
    res @?= MergeResChanged (toV 'a') (toV 'c') (toV 'a')
    ufSize uf @?= 2
    ufTotalSize uf @?= 3
  applyTestS ufRoots $ \rs _ -> rs @?= setV "ab"
  applyTestS ufMembers $ \rs _ -> rs @?= fromListIntLikeMap [(toV 'a', setV "ac"), (toV 'b', setV "b")]
  applyTestS (ufMerge (toV 'c') (toV 'a')) $ \res _ -> res @?= MergeResUnchanged (toV 'a')
  applyTestS (ufMerge (toV 'b') (toV 'z')) $ \res _ -> res @?= MergeResMissing (toV 'z')

testUfRec :: TestTree
testUfRec = testCase "UF rec" $ runUF $ do
  applyS (ufAdd (toV 'a'))
  applyS (ufAdd (toV 'b'))
  applyS (ufAdd (toV 'c'))
  applyS_ (ufMerge (toV 'b') (toV 'c'))
  applyS_ (ufMerge (toV 'a') (toV 'c'))
  testS $ \uf -> do
    ufSize uf @?= 1
    ufTotalSize uf @?= 3
  applyTestS ufRoots $ \rs _ -> rs @?= setV "a"
  applyTestS ufMembers $ \rs _ -> rs @?= fromListIntLikeMap [(toV 'a', setV "abc")]

testUf :: TestTree
testUf = testGroup "UF" [testUfSimple, testUfRec]

type EG = EGraph () ArithF

runEG :: StateT EG IO () -> IO ()
runEG = runS egNew

noA :: EAnalysisOff ArithF
noA = EAnalysisOff

testEgSimple :: TestTree
testEgSimple = testCase "EG simple" $ runEG $ do
  -- We're going to have our egraph track the equality `2 + 2 = 4`.
  -- Some simple terms:
  let termFour = ArithConst 4
      termTwo = ArithConst 2
      termPlus = ArithPlus termTwo termTwo
  -- Test that the empty egraph is sane
  testS $ \eg -> do
    egClassSize eg @?= 0
    egTotalClassSize eg @?= 0
    egNodeSize eg @?= 0
    egNeedsRebuild eg @?= False
  -- Add the term `4`
  cidFour <- applyTestS (egAddTerm noA termFour) $ \(c, x) eg -> do
    c @?= ChangedYes
    egClassSize eg @?= 1
    egTotalClassSize eg @?= 1
    egNodeSize eg @?= 1
    egNeedsRebuild eg @?= False
    pure x
  -- Add the term `2`
  cidTwo <- applyTestS (egAddTerm noA termTwo) $ \(c, x) eg -> do
    c @?= ChangedYes
    x @/= cidFour
    egClassSize eg @?= 2
    egTotalClassSize eg @?= 2
    egNodeSize eg @?= 2
    egNeedsRebuild eg @?= False
    pure x
  -- Add the term `4` again and assert things haven't changed
  applyTestS (egAddTerm noA termFour) $ \(c, x) eg -> do
    c @?= ChangedNo
    x @?= cidFour
    egClassSize eg @?= 2
    egTotalClassSize eg @?= 2
    egNodeSize eg @?= 2
    egNeedsRebuild eg @?= False
  -- Add the term `2 + 2`
  cidPlus <- applyTestS (egAddTerm noA termPlus) $ \(c, x) eg -> do
    c @?= ChangedYes
    x @/= cidFour
    x @/= cidTwo
    egClassSize eg @?= 3
    egTotalClassSize eg @?= 3
    egNodeSize eg @?= 3
    egNeedsRebuild eg @?= False
    pure x
  -- Merge `4` and `4` and assert things haven't changed
  applyTestS (egMerge noA cidFour cidFour) $ \m eg -> do
    egNeedsRebuild eg @?= False
    case m of
      Nothing -> fail "Could not resolve cidFour"
      Just (c, x) -> do
        c @?= ChangedNo
        x @?= cidFour
  -- Merge `2 + 2` and `4`
  cidMerged <- applyTestS (egMerge noA cidPlus cidFour) $ \m eg -> do
    egNeedsRebuild eg @?= True
    case m of
      Nothing -> fail "Could not resolve one of cidFour or cidPlus"
      Just (c, x) -> do
        c @?= ChangedYes
        unless (x == cidFour || x == cidPlus) (fail "Merged class should be one of inputs")
        x @/= cidTwo
        pure x
  pure ()

testEg :: TestTree
testEg = testGroup "EG" [testEgSimple]

main :: IO ()
main = defaultMain (testGroup "Overeasy" [testUf , testEg])
