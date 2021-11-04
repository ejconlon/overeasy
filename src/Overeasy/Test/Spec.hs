module Overeasy.Test.Spec (main) where

import Control.Monad (void, when)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalStateT, execState, execStateT, runState, evalState)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.List (delete)
import Data.Maybe (isJust, fromJust)
import Hedgehog (Gen, Range, forAll, property, (/==), (===), PropertyT, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisOff (..), EClassId (..), EGraph (..), ENodeId (..), egAddTerm, egClassSize, egFindTerm, egMerge,
                        egNeedsRebuild, egNew, egNodeSize, egRebuild, egTotalClassSize, egWorkList, egCanonicalize, EAnalysis (..), egClassInfo, eciData)
import Overeasy.Expressions.BinTree (pattern BinTreeBranch, pattern BinTreeLeaf, BinTree, BinTreeF (..))
import qualified Overeasy.IntLike.Equiv as ILE
import qualified Overeasy.IntLike.Graph as ILG
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Test.Arith (ArithF, pattern ArithConst, pattern ArithPlus)
import Overeasy.Test.Assertions ((@/=), assertFalse, assertTrue)
import Overeasy.UnionFind (MergeRes (..), UnionFind (..), ufAdd, ufFind, ufMembers, ufMerge, ufNew, ufOnConflict,
                           ufRoots, ufTotalSize)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (DependencyType (..), TestTree, after, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Test.Tasty.Hedgehog (testProperty)
import Data.Functor.Foldable (cata)
import Data.Semigroup (Max(..))
import Overeasy.Assoc (assocBwd, assocFwd, assocNeedsClean, Assoc, assocNew, assocAdd, assocSize, assocUpdate, assocDeadFwd, assocDeadBwd, assocClean, assocFromPairs)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Control.Monad.IO.Class (liftIO)
import Text.Pretty.Simple (pPrint)
import GHC.Stack (HasCallStack)
import qualified Data.HashSet as HashSet
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import Data.Bifunctor (bimap)

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
  deriving newtype (Eq, Ord, Hashable)

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

genListOfDistinctPairs :: Eq a => Range Int -> [a] -> Gen [(a, a)]
genListOfDistinctPairs nOpsRange vs =
  if length vs < 2
    then pure []
    else Gen.list nOpsRange (genDistinctPairFromList vs)

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
        nOpsRange = Range.linear 0 (nMembers * nMembers)
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

type EGA = EGraph () ArithF

runEGA :: StateT EGA IO () -> IO ()
runEGA = runS egNew

noA :: EAnalysisOff ArithF
noA = EAnalysisOff

type AV = Assoc ENodeId V

assertAssocInvariants :: HasCallStack => AV -> Assertion
assertAssocInvariants av = do
  let fwd = assocFwd av
      bwd = assocBwd av
  -- Assert that the assoc has been rebuilt
  assertFalse (assocNeedsClean av)
  -- Look at sizes to confirm that assoc could map 1-1
  ILM.size fwd @?= HashMap.size bwd
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(x, fc) ->
    HashMap.lookup fc bwd @?= Just x
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(fc, x) ->
    ILM.lookup x fwd @?= Just fc

data AssocCase = AssocCase !String ![(Int, Char)] ![(Int, Char, Int)] ![(Int, Char)]

allAssocCases :: [AssocCase]
allAssocCases =
  let start = [(0, 'a'), (1, 'b'), (2, 'c')]
  in [ AssocCase "base" start [] start
     , AssocCase "ident" start [(0, 'a', 0)] start
     , AssocCase "superfluous" start [(4, 'a', 0)] start
     , AssocCase "internal" start [(0, 'b', 1)] [(1, 'b'), (2, 'c')]
     , AssocCase "external" start [(0, 'd', 0)] [(0, 'd'), (1, 'b'), (2, 'c')]
     , AssocCase "additional" start [(4, 'd', 4)] [(0, 'a'), (1, 'b'), (2, 'c'), (4, 'd')]
     , AssocCase "chain fwd" start [(0, 'b', 1), (1, 'c', 2)] [(2, 'c')]
     , AssocCase "chain bwd" start [(1, 'c', 2), (0, 'b', 2)] [(2, 'c')]
     ]

mkAssoc :: MonadFail m => [(Int, Char)] -> m AV
mkAssoc rawPairs = do
  let pairs = fmap (bimap ENodeId toV) rawPairs
      start = ENodeId 0
  case assocFromPairs start pairs of
    Just assoc -> pure assoc
    Nothing -> fail "Bad pairs"

testAssocCase :: AssocCase -> TestTree
testAssocCase (AssocCase name start act end) = testCase name $ do
  a0 <- mkAssoc start
  assertAssocInvariants a0
  assocSize a0 @?= length start


testAssocCases :: TestTree
testAssocCases = testGroup "Assoc case" (fmap testAssocCase allAssocCases)

testAssocUnit :: TestTree
testAssocUnit = testCase "Assoc unit" $ do
  let a0 = assocNew (ENodeId 0)
  assertAssocInvariants a0
  assocSize a0 @?= 0
  let members = [toV 'a', toV 'b', toV 'c'] :: [V]
  let a1 = execState (for_ members assocAdd) a0
  assertAssocInvariants a1
  assocSize a1 @?= 3
  putStrLn "========== INITIAL =========="
  pPrint a1
  let aVal = toV 'a'
      aKey = fromJust (HashMap.lookup aVal (assocBwd a1))
      bVal = toV 'b'
      bKey = fromJust (HashMap.lookup bVal (assocBwd a1))
  putStrLn "========== UPDATE ========="
  pPrint aKey
  pPrint bVal
  let (newAKey, a2) = runState (assocUpdate aKey bVal) a1
  newAKey @?= bKey
  putStrLn "========== BEFORE CLEAN =========="
  pPrint a2
  assertTrue (assocNeedsClean a2)
  assocDeadFwd a2 @?= ILS.fromList [aKey]
  assocDeadBwd a2 @?= HashSet.fromList [aVal]
  let a3 = execState assocClean a2
  putStrLn "========== AFTER CLEAN =========="
  pPrint a3
  assertAssocInvariants a3
  assocSize a3 @?= 2

testEgUnit :: TestTree
testEgUnit = after AllSucceed "Assoc unit" $ testCase "EG unit" $ runEGA $ do
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
  testS pPrint
  applyTestS (egRebuild noA) $ \() eg -> do
    pPrint eg
    egFindTerm termFour eg @?= Just cidMerged
    egFindTerm termPlus eg @?= Just cidMerged
    egFindTerm termTwo eg @?= Just cidTwo
    egNeedsRebuild eg @?= False

genBinTree :: Gen a -> Gen (BinTree a)
genBinTree genA = genEither where
  genLeaf = fmap BinTreeLeaf genA
  genBranch = Gen.subterm2 genEither genEither BinTreeBranch
  genEither = Gen.recursive Gen.choice [genLeaf] [genBranch]

genBinTreeMembers :: Int -> Gen [BinTree V]
genBinTreeMembers maxElems = Gen.list (Range.linear 0 maxElems) (genBinTree (genV maxElems))

type EGD = Max V
type EGF = BinTreeF V
type EGT = BinTree V
type EGV = EGraph EGD EGF
data MaxV = MaxV

analyzeBinTree :: Semigroup m => (a -> m) -> BinTree a -> m
analyzeBinTree f = cata go where
  go = \case
    BinTreeLeafF a -> f a
    BinTreeBranchF x y -> x <> y

maxBinTreeLeaf :: Ord a => BinTree a -> a
maxBinTreeLeaf = getMax . analyzeBinTree Max

instance EAnalysis EGD EGF MaxV where
  eaMake _ fc eg = case fc of
    BinTreeLeafF v -> Max v
    BinTreeBranchF c1 c2 ->
      let v1 = eciData (fromJust (egClassInfo c1 eg))
          v2 = eciData (fromJust (egClassInfo c2 eg))
      in v1 <> v2
  eaJoin _ v1 v2 = v1 <> v2
  eaModify _ _ g = g


propEgInvariants :: (Show d, Traversable f, Eq (f EClassId), Hashable (f EClassId), Show (f EClassId)) => EGraph d f -> PropertyT IO ()
propEgInvariants eg = do
  --- XXX
  liftIO (putStrLn "========================")
  liftIO (pPrint eg)
  -- Invariants require that no rebuild is needed (empty worklist)
  assert (not (egNeedsRebuild eg))
  -- First look at node assoc (NodeId <-> f ClassId)
  let assoc = egNodeAssoc eg
      fwd = assocFwd assoc
      bwd = assocBwd assoc
  -- Assert that the assoc has been rebuilt
  assert (not (assocNeedsClean assoc))
  -- Look at sizes to confirm that assoc could map 1-1
  ILM.size fwd === HashMap.size bwd
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(x, fc) ->
    HashMap.lookup fc bwd === Just x
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(fc, x) ->
    ILM.lookup x fwd === Just fc
  -- Now look at hashcons (NodeId <-> ClassId)
  let hc = egHashCons eg
  ILM.size hc === ILM.size fwd
  -- TODO assert that hashcons has exactly same keys as assoc fwd keys
  -- TODO assert that hashcons has exactly the same values as unionfind roots
  -- TODO assert that classmap has exactly the same keys as unionfind roots
  -- TODO assert that classmap class has node values that are hashconsed to class
  -- TODO assert that all node values in all classmap classes equal hc keys
  -- Now test recanonicalization
  for_ (HashMap.keys bwd) $ \fc ->
    let recanon = evalState (egCanonicalize fc) eg
    in recanon === Just fc


genNodePairs :: Range Int -> EGV -> Gen [(EClassId, EClassId)]
genNodePairs nOpsRange eg = genListOfDistinctPairs nOpsRange (ILM.keys (egClassMap eg))


testEgProp :: TestTree
testEgProp = after AllSucceed "EG unit" $ testProperty "EG prop" $
  let maxElems = 50
      eg0 = egNew :: EGV
  in property $ do
    assert (egNodeSize eg0 == 0)
    assert (egClassSize eg0 == 0)
    -- propEgInvariants eg0
    -- --- XXX add forAlls back
    -- -- members <- forAll (genBinTreeMembers maxElems)
    -- let members = [BinTreeLeaf (toV 'a'), BinTreeLeaf (toV 'b'), BinTreeLeaf (toV 'c')] :: [EGT]
    -- let nMembers = length members
    --     nOpsRange = Range.linear 0 (nMembers * nMembers)
    -- let eg1 = execState (for_ members (egAddTerm MaxV)) eg0
    -- propEgInvariants eg1
    -- assert (egNodeSize eg1 >= 0)
    -- egClassSize eg1 === egNodeSize eg1
    -- execState (egRebuild MaxV) eg1 === eg1
    -- -- pairs <- forAll (genNodePairs nOpsRange eg1)
    -- let pairs = [(EClassId 0, EClassId 1)]
    -- let eg2 = execState (for_ pairs (uncurry (egMerge MaxV))) eg1
    -- egNodeSize eg2 === egNodeSize eg1
    -- egNeedsRebuild eg2 === not (null pairs)
    -- let eg3 = execState (egRebuild MaxV) eg2
    -- egNodeSize eg3 === egNodeSize eg2
    -- propEgInvariants eg3

main :: IO ()
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  defaultMain $ testGroup "Overeasy"
    -- [ testUfUnit
    -- , testEgUnit
    [ testAssocCases
    , testAssocUnit
    -- , testUfProp
    -- , testEgProp
    ]
