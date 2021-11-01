module Overeasy.Test.Spec (main) where

import Control.Monad (void, when)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalStateT, execState, execStateT, runState)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.List (delete)
import Data.Maybe (isJust)
import Hedgehog (Gen, Range, forAll, property, (/==), (===), Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisOff (..), EClassId (..), EGraph, egAddTerm, egClassSize, egFindTerm, egMerge,
                        egNeedsRebuild, egNew, egNodeSize, egRebuild, egTotalClassSize, egWorkList)
import Overeasy.Expressions.BinTree (pattern BinTreeBranch, pattern BinTreeLeaf, BinTree, BinTreeF (..))
import qualified Overeasy.IntLike.Equiv as ILE
import qualified Overeasy.IntLike.Graph as ILG
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Test.Arith (ArithF, pattern ArithConst, pattern ArithPlus)
import Overeasy.Test.Assertions ((@/=))
import Overeasy.UnionFind (MergeRes (..), UnionFind (..), ufAdd, ufFind, ufMembers, ufMerge, ufNew, ufOnConflict,
                           ufRoots, ufTotalSize)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (DependencyType (..), TestTree, after, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Data.Functor.Foldable (cata)
import Data.Semigroup (Max(..))
import Overeasy.Assoc (Assoc)

applyS :: Monad m => State s a -> StateT s m a
applyS = state . runState

applyS_ :: Monad m => State s a -> StateT s m ()
applyS_ = void . applyS

testS :: Monad m => (s -> m a) -> StateT s m a
testS p = get >>= lift . p

applyTestS :: Monad m => State s a -> (a -> s -> m b) -> StateT s m b
applyTestS act check = do
  a <- applyS act
  s <- get
  lift (check a s)

foldS_ :: (Monad m, Foldable t) => s -> t a -> (a -> StateT s m ()) -> m s
foldS_ z as f = execStateT (for_ as f) z

runS :: Monad m => s -> StateT s m () -> m ()
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
setV = ILS.fromList . fmap toV

type UF = UnionFind V

runUF :: StateT UF IO () -> IO ()
runUF = runS ufNew

testUfSimple :: TestTree
testUfSimple = testCase "UF simple" $ runUF $ do
  testS $ \uf -> ufSize uf @?= 0
  testS $ \uf -> ufTotalSize uf @?= 0
  applyTestS ufRoots $ \rs _ -> rs @?= ILS.empty
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
  applyTestS ufMembers $ \rs _ -> rs @?= ILM.fromList [(toV 'a', setV "ac"), (toV 'b', setV "b")]
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
  applyTestS ufMembers $ \rs _ -> rs @?= ILM.fromList [(toV 'a', setV "abc")]

testUfUnit :: TestTree
testUfUnit = testGroup "UF unit" [testUfSimple, testUfRec]

genDistinctPairFromList :: Eq a => [a] -> Gen (a, a)
genDistinctPairFromList = \case
  xs@(_:_:_) -> do
    a <- Gen.element xs
    b <- Gen.element (delete a xs)
    pure (a, b)
  _ -> error "List needs more than two elements"

genV :: Int -> Gen V
genV maxElems =
  let minVal = ord 'a'
      maxVal = minVal + maxElems * maxElems
  in fmap V (Gen.int (Range.linear minVal maxVal))

genMembers :: Int -> Gen [V]
genMembers maxElems = do
  let nElemsRange = Range.linear 0 maxElems
      minVal = ord 'a'
  n <- Gen.int nElemsRange
  pure (fmap (\i -> V (minVal + i)) [0..n-1])

genListOfDistinctPairs :: Range Int -> [V] -> Gen [(V, V)]
genListOfDistinctPairs nOpsRange vs =
  if length vs < 2
    then pure []
    else Gen.list nOpsRange (genDistinctPairFromList vs)

mkInitUf :: [V] -> UF
mkInitUf vs = execState (for_ vs ufAdd) ufNew

mkMergedUf :: [(V, V)] -> UF -> UF
mkMergedUf vvs = execState (for_ vvs (uncurry ufMerge))

testUfProp :: TestTree
testUfProp = after AllSucceed "UF unit" $ testProperty "UF prop" $
  let maxElems = 50
  in property $ do
    -- generate elements
    memberList <- forAll (genMembers maxElems)
    let memberSet = ILS.fromList memberList
        nMembers = ILS.size memberSet
        allPairs = ILS.unorderedPairs memberSet
        nOpsRange = Range.constant 0 (nMembers * nMembers)
    let initUf = mkInitUf memberList
    -- assert that sizes indicate nothing is merged
    ufSize initUf === nMembers
    ufTotalSize initUf === nMembers
    -- assert that find indicates nothing is merged
    for_ allPairs $ \(a, b) -> flip evalStateT initUf $ do
      x <- applyS (ufFind a)
      y <- applyS (ufFind b)
      lift (x /== y)
    -- generate some pairs and merge them
    mergePairs <- forAll (genListOfDistinctPairs nOpsRange memberList)
    let mergedUf = mkMergedUf mergePairs initUf
    -- assert that total size is unchanged
    ufTotalSize mergedUf === nMembers
    -- calculate components by graph reachability
    let components = ILG.undirectedComponents mergePairs
    -- assert that elements are equal or not according to component
    void $ foldS_ mergedUf allPairs $ \(a, b) -> do
      x <- applyS (ufFind a)
      y <- applyS (ufFind b)
      let aComponent = ILE.lookupClass a components
          bComponent = ILE.lookupClass b components
      if isJust aComponent && aComponent == bComponent
        then lift (x === y)
        else lift (x /== y)

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
    egFindTerm termFour eg @?= Just x
    egClassSize eg @?= 1
    egTotalClassSize eg @?= 1
    egNodeSize eg @?= 1
    egNeedsRebuild eg @?= False
    pure x
  -- Add the term `2`
  cidTwo <- applyTestS (egAddTerm noA termTwo) $ \(c, x) eg -> do
    c @?= ChangedYes
    x @/= cidFour
    egFindTerm termTwo eg @?= Just x
    egClassSize eg @?= 2
    egTotalClassSize eg @?= 2
    egNodeSize eg @?= 2
    egNeedsRebuild eg @?= False
    pure x
  -- Add the term `4` again and assert things haven't changed
  applyTestS (egAddTerm noA termFour) $ \(c, x) eg -> do
    c @?= ChangedNo
    x @?= cidFour
    egFindTerm termFour eg @?= Just x
    egClassSize eg @?= 2
    egTotalClassSize eg @?= 2
    egNodeSize eg @?= 2
    egNeedsRebuild eg @?= False
  -- Add the term `2 + 2`
  cidPlus <- applyTestS (egAddTerm noA termPlus) $ \(c, x) eg -> do
    c @?= ChangedYes
    x @/= cidFour
    x @/= cidTwo
    egFindTerm termPlus eg @?= Just x
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
        egFindTerm termFour eg @?= Just x
  -- Merge `2 + 2` and `4`
  cidMerged <- applyTestS (egMerge noA cidPlus cidFour) $ \m eg -> do
    let cidExpected = ufOnConflict cidPlus cidFour
    egNeedsRebuild eg @?= True
    egWorkList eg @?= ILS.singleton cidExpected
    case m of
      Nothing -> fail "Could not resolve one of cidFour or cidPlus"
      Just (c, x) -> do
        c @?= ChangedYes
        x @?= cidExpected
        x @/= cidTwo
        pure x
  -- Now rebuild
  applyTestS (egRebuild noA) $ \() eg -> do
    egFindTerm termFour eg @?= Just cidMerged
    egFindTerm termPlus eg @?= Just cidMerged
    egFindTerm termTwo eg @?= Just cidTwo
    egNeedsRebuild eg @?= False

testEgUnit :: TestTree
testEgUnit = testGroup "EG unit" [testEgSimple]

genBinTree :: Gen a -> Gen (BinTree a)
genBinTree genA = genEither where
  genLeaf = fmap BinTreeLeaf genA
  genBranch = Gen.subterm2 genEither genEither BinTreeBranch
  genEither = Gen.recursive Gen.choice [genLeaf] [genBranch]

analyzeBinTree :: Semigroup m => (a -> m) -> BinTree a -> m
analyzeBinTree f = cata go where
  go = \case
    BinTreeLeafF a -> f a
    BinTreeBranchF x y -> x <> y

maxBinTreeLeaf :: Ord a => BinTree a -> a
maxBinTreeLeaf = getMax . analyzeBinTree Max

propAssocIsCanonical :: Assoc x a -> Property
propAssocIsCanonical assoc = undefined

-- TODO define egraph invariants:
-- assoc and hashcons must have canonical nodes only?

testEgProp :: TestTree
testEgProp = after AllSucceed "EG unit" $ testProperty "EG prop" $
  let maxElems = 50
  in property $ do
    -- TODO!
    -- Gen a list of `genBinTreeV`
    -- explode them into an assoc and note the unique size
    -- add them to an empty egraph, assert size eq and does not need rebuild
    -- assert egraph invariants
    -- pick pairs to merge
    -- for sequential groups of elems, merge, and after all rebuild
    -- assert egraph invariants
    pure ()

main :: IO ()
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug (setEnv "TASTY_NUM_THREADS" "1")
  defaultMain $ testGroup "Overeasy"
    [ testUfUnit
    , testEgUnit
    , testUfProp
    , testEgProp
    ]
