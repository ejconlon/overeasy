module Overeasy.Test.Spec (main) where

import Control.Monad (foldM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalStateT, execState, runState)
import Data.Char (chr, ord)
import Data.Coerce (Coercible)
import Data.Foldable (for_)
import Data.List (delete)
import Data.Traversable (for)
import Hedgehog (Gen, Range, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisOff (..), EClassId (..), EGraph, egAddTerm, egClassSize, egFindTerm, egMerge,
                        egNeedsRebuild, egNew, egNodeSize, egRebuild, egTotalClassSize, egWorkList)
import Overeasy.IntLikeMap (IntLikeMap)
import qualified Overeasy.IntLikeMap as ILM
import Overeasy.IntLikeSet (IntLikeSet)
import qualified Overeasy.IntLikeSet as ILS
import Overeasy.Test.Arith (ArithF, pattern ArithConst, pattern ArithPlus)
import Overeasy.Test.Assertions ((@/=))
import Overeasy.UnionFind (MergeRes (..), UnionFind (..), ufAdd, ufMembers, ufMerge, ufNew, ufOnConflict, ufRoots,
                           ufTotalSize)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

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

testUf :: TestTree
testUf = testGroup "UF" [testUfSimple, testUfRec]

newtype UFGroup = UFGroup { unUFGroup :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data UFTruth = UFTruth
  { ufTruthGroups :: IntLikeMap UFGroup (IntLikeSet V)
  , ufTruthMembership :: IntLikeMap V UFGroup
  } deriving stock (Eq, Show)

mkUFTruth :: MonadFail m => [[V]] -> m UFTruth
mkUFTruth = foldM goGroup (UFTruth ILM.empty ILM.empty) . zip [1..] where
  goGroup uf (i, vs) = foldM (goSingle i) uf vs
  goSingle i (UFTruth groups memship) v = do
    let g = UFGroup i
    case ILM.lookup v memship of
      Just h -> fail ("Duplicate membership for " ++ show v ++ " " ++ show g ++ " " ++ show h)
      Nothing ->
        let set' = maybe (ILS.singleton v) (ILS.insert v) (ILM.lookup g groups)
            groups' = ILM.insert g set' groups
            memship' = ILM.insert v g memship
        in pure (UFTruth groups' memship')

data UFState = UFState
  { ufStateTruth :: !UFTruth
  , ufStateFresh :: !(IntLikeSet V)
  , ufStateAdded :: !(IntLikeSet V)
  } deriving stock (Eq, Show)

mkUFState :: UFTruth -> UFState
mkUFState t = UFState t (ILS.fromList (ILM.keys (ufTruthMembership t))) ILS.empty

hasAnyV :: UFState -> Bool
hasAnyV = not . ILM.null . ufTruthMembership . ufStateTruth

-- error on no elems!
genAnyV :: UFState -> Gen V
genAnyV = Gen.element . ILM.keys . ufTruthMembership . ufStateTruth

genDistinctPairFromList :: Eq a => [a] -> Gen (a, a)
genDistinctPairFromList = \case
  xs@(_:_:_) -> do
    a <- Gen.element xs
    b <- Gen.element (delete a xs)
    pure (a, b)
  _ -> error "List needs more than two elements"

genElemFromIntLikeSet :: Coercible a Int => IntLikeSet a -> Gen a
genElemFromIntLikeSet = Gen.element . ILS.toList

hasFreshV :: UFState -> Bool
hasFreshV = not . ILS.null . ufStateFresh

-- error on no fresh elems!
genFreshV :: UFState -> Gen V
genFreshV = genElemFromIntLikeSet . ufStateFresh

hasAddedV :: UFState -> Bool
hasAddedV = not . ILS.null . ufStateFresh

-- error on no added elems!
genAddedV :: UFState -> Gen V
genAddedV s = genElemFromIntLikeSet (ufStateAdded s)

hasPairAddedV :: UFState -> Bool
hasPairAddedV s = ILS.size (ufStateAdded s) >= 2

-- error on < 2 added elems!
genPairAddedV :: UFState -> Gen (V, V)
genPairAddedV = genDistinctPairFromList . ILS.toList . ufStateAdded

genIntLikeSet :: Coercible a Int => Range Int -> Gen a -> Gen (IntLikeSet a)
genIntLikeSet r = fmap ILS.fromList . Gen.list r

-- genSubIntLikeSet :: Coercible Int a -> IntLikeSet a -> Gen (IntLikeSet a)

genV :: Gen V
genV = fmap V (Gen.int (Range.linear (ord 'a') maxBound))

insertMM :: (Coercible k Int, Coercible v Int) => (v, k) -> IntLikeMap k (IntLikeSet v) -> IntLikeMap k (IntLikeSet v)
insertMM (v, k) = ILM.insertWith (<>) k (ILS.singleton v)

genUfTruth :: Int -> Range Int -> Gen UFTruth
genUfTruth maxElems assignedClassRange = do
  let nElemsRange = Range.linear 0 maxElems
  memberList <- Gen.list nElemsRange ((\a b -> (a, UFGroup b)) <$> genV <*> Gen.int assignedClassRange)
  let members = ILM.fromList memberList
      groups = foldr insertMM ILM.empty (ILM.toList members)
  pure (UFTruth groups members)

genAnyPairs :: Int -> UFTruth -> Gen [(V, V)]
genAnyPairs maxOps = genOnlyPairs maxOps . ILM.keys . ufTruthMembership

genOnlyPairs :: Int -> [V] -> Gen [(V, V)]
genOnlyPairs maxOps vs =
  if length vs < 2
    then pure []
    else
      let nOpsRange = Range.linear 0 maxOps
      in Gen.list nOpsRange (genDistinctPairFromList vs)

genMergePairs :: Int -> Range Int -> UFTruth -> Gen [(V, V)]
genMergePairs maxOps assignedClassRange t = do
  let nOpsRange = Range.linear 0 maxOps
      gvs = fmap ILS.toList (ufTruthGroups t)
  groups <- Gen.list nOpsRange (fmap UFGroup (Gen.int assignedClassRange))
  let okGroups = filter (\g -> maybe False (\s -> ILS.size s >= 2) (ILM.lookup g (ufTruthGroups t))) groups
  for okGroups (\g -> genDistinctPairFromList (ILM.partialLookup g gvs))

mkInitUf :: UFTruth -> UF
mkInitUf t =
  let vs = ILM.keys (ufTruthMembership t)
  in execState (for_ vs ufAdd) ufNew

mkMergedUf :: [(V, V)] -> UF -> UF
mkMergedUf vvs = execState (for_ vvs (uncurry ufMerge))

testUfProp :: TestTree
testUfProp = testProperty "UF Prop" $
  let maxClasses = 10
      maxElems = 100
      maxOps = 30
      nClassesRange = Range.linear 1 maxClasses
  in property $ do
    nClasses <- forAll (Gen.int nClassesRange)
    let assignedClassRange = Range.constant 0 (nClasses - 1)
    ufTruth <- forAll (genUfTruth maxElems assignedClassRange)
    let initUf = mkInitUf ufTruth
        memberSet = ILS.fromList (ILM.keys (ufTruthMembership ufTruth))
    let nMembers = ILS.size memberSet
    ufSize initUf === nMembers
    ufTotalSize initUf === nMembers
    mergePairs <- forAll (genMergePairs maxOps assignedClassRange ufTruth)
    -- let expectedMerged = foldr ILS.insert ILS.empty (mergePairs >>= \(x, y) -> [x, y])
    let mergedUf = mkMergedUf mergePairs initUf
    -- TODO
    ufTotalSize mergedUf === nMembers

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
  pure ()

testEg :: TestTree
testEg = testGroup "EG" [testEgSimple]

testEgProp :: TestTree
testEgProp = testGroup "EG prop" []

main :: IO ()
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug (setEnv "TASTY_NUM_THREADS" "1")
  defaultMain $ testGroup "Overeasy"
    [ testUf
    , testEg
    , testUfProp
    , testEgProp
    ]
