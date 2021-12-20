module Main (main) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalState, evalStateT, execState, execStateT, gets,
                                   runState)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (bimap)
import Data.Char (chr, ord)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import Data.Semigroup (Max (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Hedgehog (Gen, Range, forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Overeasy.Assoc (Assoc, AssocInsertRes (..), assocBwd, assocCanCompact, assocCompact, assocEquiv, assocFromList,
                       assocFwd, assocInsert, assocLeaves, assocLookupRoot, assocNew, assocPartialLookupByKey,
                       assocRoots, assocSize)
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisAlgebra (..), EAnalysisOff (..), EClassId (..), EClassInfo (..), EGraph (..),
                        ENodeId (..), egAddTerm, egCanonicalize, egClassSize, egCompact, egFindTerm, egMerge,
                        egMergeMany, egNeedsRebuild, egNew, egNodeSize, egRebuild, egWorkList)
import Overeasy.EquivFind (EquivFind (..), efAdd, efCanCompact, efCompact, efFindRoot, efLeaves, efLeavesSize, efMerge,
                           efMergeSets, efNew, efRoots, efRootsSize, efTotalSize)
import Overeasy.Expressions.BinTree (BinTree, BinTreeF (..), pattern BinTreeBranch, pattern BinTreeLeaf)
import qualified Overeasy.IntLike.Equiv as ILE
import qualified Overeasy.IntLike.Graph as ILG
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Test.Overeasy.Arith (Arith (..), ArithF)
import Test.Overeasy.Assertions (MonadTest, TestLimit, assert, setupTests, testGen, testUnit, (/==), (===))
import Test.Tasty (DependencyType (..), TestTree, after, defaultMain, testGroup)

fullyEvaluate :: (MonadIO m, NFData a) => a -> m a
fullyEvaluate = liftIO . evaluate . force

applyS :: Monad m => State s a -> StateT s m a
applyS = state . runState

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

flipFoldM :: Monad m => b -> [a] -> (b -> a -> m b) -> m b
flipFoldM b as f = foldM f b as

newtype V = V { unV :: Int }
  deriving newtype (Eq, Ord, Hashable, NFData)

instance Show V where
  show = show . fromV

toV :: Char -> V
toV = V . ord

fromV :: V -> Char
fromV = chr . unV

setV :: String -> IntLikeSet V
setV = ILS.fromList . fmap toV

mapV :: [(Char, Char)] -> IntLikeMap V V
mapV = ILM.fromList . fmap (bimap toV toV)

multiMapV :: [(Char, String)] -> IntLikeMap V (IntLikeSet V)
multiMapV = ILM.fromList . fmap (bimap toV setV)

type UF = EquivFind V

testUfSimple :: TestTree
testUfSimple = testUnit "UF simple" $ runS efNew $ do
  testS $ \ef -> do
    efRootsSize ef === 0
    efLeavesSize ef === 0
    efTotalSize ef === 0
    efRoots ef === []
    efLeaves ef === []
    efFwd ef === ILM.empty
    efBwd ef === ILM.empty
  _ <- applyS (efAdd (toV 'a'))
  testS $ \ef -> do
    efRootsSize ef === 1
    efLeavesSize ef === 0
    efTotalSize ef === 1
    ILS.fromList (efRoots ef) === setV "a"
    ILS.fromList (efLeaves ef) === ILS.empty
    efFwd ef === multiMapV [('a', "")]
    efBwd ef === ILM.empty
  _ <- applyS (efAdd (toV 'b'))
  _ <- applyS (efAdd (toV 'c'))
  testS $ \ef -> do
    efRootsSize ef === 3
    efLeavesSize ef === 0
    efTotalSize ef === 3
    ILS.fromList (efRoots ef) === setV "abc"
    ILS.fromList (efLeaves ef) === ILS.empty
    efFwd ef === multiMapV [('a', ""), ('b', ""), ('c', "")]
    efBwd ef === ILM.empty
  applyTestS (efMerge (toV 'a') (toV 'c')) $ \res ef -> do
    res === Just (toV 'a', setV "c")
    efRootsSize ef === 2
    efLeavesSize ef === 1
    efTotalSize ef === 3
    ILS.fromList (efRoots ef) === setV "ab"
    ILS.fromList (efLeaves ef) === setV "c"
    efFwd ef === multiMapV [('a', "c"), ('b', "")]
    efBwd ef === mapV [('c', 'a')]
  applyTestS (efMerge (toV 'c') (toV 'a')) $ \res _ -> res === Nothing
  applyTestS (efMerge (toV 'b') (toV 'z')) $ \res _ -> res === Nothing

testUfRec :: TestTree
testUfRec = testUnit "UF rec" $ runS efNew $ do
  _ <- applyS (efAdd (toV 'a'))
  _ <- applyS (efAdd (toV 'b'))
  _ <- applyS (efAdd (toV 'c'))
  applyTestS (efMerge (toV 'b') (toV 'c')) $ \res ef -> do
    res === Just (toV 'b', setV "c")
    efRootsSize ef === 2
    efLeavesSize ef === 1
    efTotalSize ef === 3
    ILS.fromList (efRoots ef) === setV "ab"
    ILS.fromList (efLeaves ef) === setV "c"
    efFwd ef === multiMapV [('a', ""), ('b', "c")]
    efBwd ef === mapV [('c', 'b')]
  applyTestS (efMerge (toV 'a') (toV 'c')) $ \res ef -> do
    res === Just (toV 'a', setV "bc")
    efRootsSize ef === 1
    efLeavesSize ef === 2
    efTotalSize ef === 3
    ILS.fromList (efRoots ef) === setV "a"
    ILS.fromList (efLeaves ef) === setV "bc"
    efFwd ef === multiMapV [('a', "bc")]
    efBwd ef === mapV [('b', 'a'), ('c', 'a')]

testUfMany :: TestTree
testUfMany = testUnit "UF many" $ runS efNew $ do
  _ <- applyS (efAdd (toV 'a'))
  _ <- applyS (efAdd (toV 'b'))
  _ <- applyS (efAdd (toV 'c'))
  _ <- applyS (efAdd (toV 'd'))
  _ <- applyS (efAdd (toV 'e'))
  applyTestS (efMergeSets [setV "cde"]) $ \res ef -> do
    res === Just (setV "c", setV "de")
    efRootsSize ef === 3
    efLeavesSize ef === 2
    efTotalSize ef === 5
    ILS.fromList (efRoots ef) === setV "abc"
    ILS.fromList (efLeaves ef) === setV "de"
    efFwd ef === multiMapV [('a', ""), ('b', ""), ('c', "de")]
    efBwd ef === mapV [('d', 'c'), ('e', 'c')]
  applyTestS (efMergeSets [setV "abd"]) $ \res ef -> do
    res === Just (setV "a", setV "bcde")
    efRootsSize ef === 1
    efLeavesSize ef === 4
    efTotalSize ef === 5
    ILS.fromList (efRoots ef) === setV "a"
    ILS.fromList (efLeaves ef) === setV "bcde"
    efFwd ef === multiMapV [('a', "bcde")]
    efBwd ef === mapV [('b', 'a'), ('c', 'a'), ('d', 'a'), ('e', 'a')]

testUfSets :: TestTree
testUfSets = testUnit "UF sets" $ runS efNew $ do
  _ <- applyS (efAdd (toV 'a'))
  _ <- applyS (efAdd (toV 'b'))
  _ <- applyS (efAdd (toV 'c'))
  _ <- applyS (efAdd (toV 'd'))
  _ <- applyS (efAdd (toV 'e'))
  applyTestS (efMergeSets [setV "cde", setV "abc"]) $ \res ef -> do
    res === Just (setV "a", setV "bcde")
    efRootsSize ef === 1
    efLeavesSize ef === 4
    efTotalSize ef === 5
    ILS.fromList (efRoots ef) === setV "a"
    efFwd ef === multiMapV [('a', "bcde")]

testUfCompact :: TestTree
testUfCompact = testUnit "UF compact" $ runS efNew $ do
  _ <- applyS (efAdd (toV 'a'))
  _ <- applyS (efAdd (toV 'b'))
  _ <- applyS (efAdd (toV 'c'))
  _ <- applyS (efAdd (toV 'd'))
  _ <- applyS (efAdd (toV 'e'))
  testS $ \ef -> assert (not (efCanCompact ef))
  applyTestS (efMergeSets [setV "cde"]) $ \res ef -> do
    res === Just (setV "c", setV "de")
    efFwd ef === multiMapV [('a', ""), ('b', ""), ('c', "de")]
    efBwd ef === mapV [('d', 'c'), ('e', 'c')]
    assert (efCanCompact ef)
  applyTestS efCompact $ \res ef -> do
    efFwd ef === multiMapV [('a', ""), ('b', ""), ('c', "")]
    efBwd ef === ILM.empty
    res === multiMapV [('c', "de")]
    assert (not (efCanCompact ef))

testUfUnit :: TestTree
testUfUnit = testGroup "UF unit" [testUfSimple, testUfRec, testUfMany, testUfSets, testUfCompact]

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
      maxVal = minVal + maxElems - 1
  in fmap V (Gen.int (Range.linear minVal maxVal))

genMembers :: Int -> Gen [V]
genMembers maxElems = do
  let nElemsRange = Range.linear 0 maxElems
      minVal = ord 'a'
  n <- Gen.int nElemsRange
  pure (fmap (\i -> V (minVal + i)) [0..n-1])

mkInitUf :: [V] -> UF
mkInitUf vs = execState (for_ vs efAdd) efNew

mkPairsMergedUf :: [(V, V)] -> UF -> UF
mkPairsMergedUf vvs = execState (for_ vvs (uncurry efMerge))

mkSetsMergedUf :: [(V, V)] -> UF -> UF
mkSetsMergedUf vvs = execState (for_ (fmap (\(x, y) -> [ILS.fromList [x, y]]) vvs) efMergeSets)

mkSingleMergedUf :: [(V, V)] -> UF -> UF
mkSingleMergedUf vvs = execState (efMergeSets (fmap (\(x, y) -> ILS.fromList [x, y]) vvs))

data MergeStrat = MergeStratPairs | MergeStratSets | MergeStratSingle
  deriving stock (Eq, Show, Enum, Bounded)

genMergeStrat :: Gen MergeStrat
genMergeStrat = Gen.enumBounded

testUfProp :: TestLimit -> TestTree
testUfProp lim = after AllSucceed "UF unit" $ testGen "UF prop" lim $ do
  let maxElems = 50
  -- generate elements
  memberList <- forAll (genMembers maxElems)
  let memberSet = ILS.fromList memberList
      nMembers = ILS.size memberSet
      allPairs = ILS.unorderedPairs memberSet
      nOpsRange = Range.linear 0 (nMembers * nMembers)
  let initUf = mkInitUf memberList
  -- assert that sizes indicate nothing is merged
  efRootsSize initUf === nMembers
  efLeavesSize initUf === 0
  efTotalSize initUf === nMembers
  -- assert that find indicates nothing is merged
  for_ allPairs $ \(a, b) -> flip evalStateT initUf $ do
    x <- applyS (gets (efFindRoot a))
    y <- applyS (gets (efFindRoot b))
    assert (isJust x)
    assert (isJust y)
    x /== y
  -- generate some pairs and merge them
  mergePairs <- forAll (genListOfDistinctPairs nOpsRange memberList)
  mergeStrat <- forAll genMergeStrat
  let mergedUf =
        case mergeStrat of
          MergeStratPairs -> mkPairsMergedUf mergePairs initUf
          MergeStratSets -> mkSetsMergedUf mergePairs initUf
          MergeStratSingle -> mkSingleMergedUf mergePairs initUf
  -- assert that total size is unchanged
  efTotalSize mergedUf === nMembers
  -- calculate components by graph reachability
  let components = ILG.undirectedComponents mergePairs
  -- assert that elements are equal or not according to component
  _ <- foldS_ mergedUf allPairs $ \(a, b) -> do
    x <- applyS (gets (efFindRoot a))
    y <- applyS (gets (efFindRoot b))
    let aComponent = ILE.lookupClass a components
        bComponent = ILE.lookupClass b components
    if isJust aComponent && aComponent == bComponent
      then x === y
      else x /== y
  pure ()

type AV = Assoc ENodeId V

-- | Asserts assoc is compact - should also check 'assertAssocInvariants'
assertAssocCompact :: (MonadTest m, Eq a, Hashable a, Show a) => Assoc ENodeId a -> m ()
assertAssocCompact av = do
  let fwd = assocFwd av
      bwd = assocBwd av
  -- Assert that the assoc has been rebuilt
  assert $ not (assocCanCompact av)
  -- Look at sizes to confirm that assoc could map 1-1
  ILM.size fwd === HashMap.size bwd
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(x, fc) -> do
    -- Assert is found in backward map AND maps back
    HashMap.lookup fc bwd === Just x
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(fc, x) ->
    -- Assert is present in forward map AND maps back
    ILM.lookup x fwd === Just fc

-- | Asserts assoc is correctly structured (compact or not)
assertAssocInvariants :: (MonadTest m, Eq a, Hashable a) => Assoc ENodeId a -> m ()
assertAssocInvariants av = do
  let fwd = assocFwd av
      bwd = assocBwd av
      equiv = assocEquiv av
  -- First check that fwd and bwd are 1-1
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(_, fc) -> do
    -- Assert is found in backward map
    assert $ HashMap.member fc bwd
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(_, x) ->
    -- Assert is present in forward map
    assert $ ILM.member x fwd
  -- Assert that fwd keys are exactly the equiv roots
  ILS.fromList (ILM.keys fwd) === ILS.fromList (efRoots equiv)

data AssocCase = AssocCase !String ![(Int, Char)] ![(Int, Char, Int, AssocInsertRes Int)] ![(Int, Char)]

allAssocCases :: [AssocCase]
allAssocCases =
  let start = [(0, 'a'), (1, 'b'), (2, 'c')]
  in [ AssocCase "base" start [] start
     , AssocCase "ident" start
        [(0, 'a', 0, AssocInsertResUnchanged)]
        start
     , AssocCase "superfluous" start
        [(4, 'a', 0, AssocInsertResMerged (ILS.singleton 4))]
        start
     , AssocCase "internal" start
        [(0, 'b', 0, AssocInsertResMerged (ILS.singleton 1))]
        [(0, 'b'), (2, 'c')]
     , AssocCase "external" start
        [(0, 'd', 0, AssocInsertResUpdated)]
        [(0, 'd'), (1, 'b'), (2, 'c')]
     , AssocCase "additional" start
        [(4, 'd', 4, AssocInsertResCreated)]
        [(0, 'a'), (1, 'b'), (2, 'c'), (4, 'd')]
     , AssocCase "chain fwd" start
        -- The singleton set in the second result is just the children (and self) of the clobbered node
        -- We don't have to lookup the old clobbered nodes for 1 bc when this is used everything will be merged
        [(0, 'b', 0, AssocInsertResMerged (ILS.singleton 1)), (1, 'c', 0, AssocInsertResMerged (ILS.singleton 2))]
        [(0, 'c')]
     , AssocCase "chain bwd" start
        -- The set in the second result is not a singleton here because it already had children
        [(1, 'c', 1, AssocInsertResMerged (ILS.singleton 2)), (0, 'c', 0, AssocInsertResMerged (ILS.fromList [1,2]))]
        [(0, 'c')]
     , AssocCase "chain self" start
        [(1, 'c', 1, AssocInsertResMerged (ILS.singleton 2)), (2, 'c', 1, AssocInsertResUnchanged)]
        [(0, 'a'), (1, 'c')]
     , AssocCase "chain change" start
        [(1, 'c', 1, AssocInsertResMerged (ILS.singleton 2)), (2, 'd', 1, AssocInsertResUpdated)]
        [(0, 'a'), (1, 'd')]
     , AssocCase "chain back id" start
        [(1, 'c', 1, AssocInsertResMerged (ILS.singleton 2)), (1, 'b', 1, AssocInsertResUpdated)]
        [(0, 'a'), (1, 'b')]
     , AssocCase "chain back del" start
        [(1, 'c', 1, AssocInsertResMerged (ILS.singleton 2)), (2, 'b', 1, AssocInsertResUpdated)]
        [(0, 'a'), (1, 'b')]
     , AssocCase "chain change rev" start
        [(2, 'd', 2, AssocInsertResUpdated), (1, 'c', 1, AssocInsertResUpdated)]
        [(0, 'a'), (1, 'c'), (2, 'd')]
     ]

mkAssoc :: [(Int, Char)] -> AV
mkAssoc rawPairs =
  let pairs = fmap (bimap ENodeId toV) rawPairs
  in assocFromList pairs

runAV :: Monad m => [(Int, Char)] -> StateT AV m () -> m ()
runAV = runS . mkAssoc

testAssocCase :: AssocCase -> TestTree
testAssocCase (AssocCase name start act end) = testUnit name $ runAV start $ do
  testS $ \av -> do
    assertAssocInvariants av
    assertAssocCompact av
    assocSize av === length start
  for_ act $ \(x, a, expectedY, expectedRes) -> do
    (actualY, actualRes) <- applyS (assocInsert (ENodeId x) (toV a))
    (actualY, actualRes) === coerce (expectedY, expectedRes)
    testS assertAssocInvariants
  _ <- applyS assocCompact
  testS $ \av -> do
    assertAssocInvariants av
    assertAssocCompact av
    assocSize av === length end
    let endAv = mkAssoc end
    assocFwd av === assocFwd endAv
    assocBwd av === assocBwd endAv

testAssocCases :: TestTree
testAssocCases = testGroup "Assoc case" (fmap testAssocCase allAssocCases)

testAssocUnit :: TestTree
testAssocUnit = testUnit "Assoc unit" $ do
  let a0 = assocNew :: AV
  assertAssocInvariants a0
  assertAssocCompact a0
  assocSize a0 === 0
  let aKey = ENodeId 0
      aVal = toV 'a'
      bKey = ENodeId 1
      bVal = toV 'b'
      cKey = ENodeId 2
      cVal = toV 'c'
  let members = [(aKey, aVal), (bKey, bVal), (cKey, cVal)]
  let a1 = execState (for_ members (uncurry assocInsert)) a0
  assertAssocInvariants a1
  assertAssocCompact a0
  assocSize a1 === 3
  assocRoots a1 === [aKey, bKey, cKey]
  assocLeaves a1 === []
  let (res, a2) = runState (assocInsert aKey bVal) a1
  res === (aKey, AssocInsertResMerged (ILS.singleton bKey))
  assertAssocInvariants a2
  assert $ assocCanCompact a2
  assocSize a2 === 2
  assocRoots a2 === [aKey, cKey]
  assocLeaves a2 === [bKey]
  let a3 = execState assocCompact a2
  assertAssocInvariants a3
  assertAssocCompact a3
  assocSize a3 === 2
  assocRoots a3 === [aKey, cKey]
  assocLeaves a3 === []

type EGA = EGraph () ArithF

testEgUnit :: TestTree
testEgUnit = after AllSucceed "Assoc unit" $ testUnit "EG unit" $ runS egNew $ do
  -- We're going to have our egraph track the equality `2 + 2 = 4`.
  -- Some simple terms:
  let termFour = ArithConst 4
      termTwo = ArithConst 2
      termPlus = ArithPlus termTwo termTwo
  -- Test that the empty egraph is sane
  testS $ \eg -> do
    egClassSize eg === 0
    egNodeSize eg === 0
    egNeedsRebuild eg === False
  -- Add the term `4`
  cidFour <- applyTestS (egAddTerm EAnalysisOff termFour) $ \(c, x) eg -> do
    c === ChangedYes
    egFindTerm termFour eg === Just x
    egClassSize eg === 1
    egNodeSize eg === 1
    egNeedsRebuild eg === False
    pure x
  -- Add the term `2`
  cidTwo <- applyTestS (egAddTerm EAnalysisOff termTwo) $ \(c, x) eg -> do
    c === ChangedYes
    x /== cidFour
    egFindTerm termTwo eg === Just x
    egClassSize eg === 2
    egNodeSize eg === 2
    egNeedsRebuild eg === False
    pure x
  -- Add the term `4` again and assert things haven't changed
  applyTestS (egAddTerm EAnalysisOff termFour) $ \(c, x) eg -> do
    c === ChangedNo
    x === cidFour
    egFindTerm termFour eg === Just x
    egClassSize eg === 2
    egNodeSize eg === 2
    egNeedsRebuild eg === False
  -- Add the term `2 + 2`
  cidPlus <- applyTestS (egAddTerm EAnalysisOff termPlus) $ \(c, x) eg -> do
    c === ChangedYes
    x /== cidFour
    x /== cidTwo
    egFindTerm termPlus eg === Just x
    egClassSize eg === 3
    egNodeSize eg === 3
    egNeedsRebuild eg === False
    pure x
  -- Merge `4` and `4` and assert things haven't changed
  applyTestS (egMerge cidFour cidFour) $ \m eg -> do
    egNeedsRebuild eg === False
    case m of
      Nothing -> fail "Could not resolve cidFour"
      Just c -> c === ChangedNo
  -- Merge `2 + 2` and `4`
  applyTestS (egMerge cidPlus cidFour) $ \m eg -> do
    egNeedsRebuild eg === True
    egWorkList eg === Seq.singleton (ILS.fromList [cidPlus, cidFour])
    case m of
      Nothing -> fail "Could not resolve one of cidFour or cidPlus"
      Just c -> c === ChangedYes
  -- Now rebuild
  applyTestS (egRebuild EAnalysisOff) $ \newRoots eg -> do
    cidMerged <-
      case ILM.keys newRoots of
        [x] -> pure x
        _ -> fail "Expected singleton root list"
    egFindTerm termFour eg === Just cidMerged
    egFindTerm termPlus eg === Just cidMerged
    egFindTerm termTwo eg === Just cidTwo
    egNeedsRebuild eg === False

type EGD = Max V
type EGF = BinTreeF V
type EGT = BinTree V
type EGV = EGraph EGD EGF

maxVAnalysis :: EAnalysisAlgebra EGD EGF
maxVAnalysis = EAnalysisAlgebra $ \case
  BinTreeLeafF v -> Max v
  BinTreeBranchF d1 d2 -> d1 <> d2

assertEgInvariants :: (MonadTest m, Traversable f, Eq (f EClassId), Hashable (f EClassId), Show (f EClassId)) => EGraph d f -> m ()
assertEgInvariants eg = do
  -- Invariants require that no rebuild is needed (empty worklist)
  assert $ not (egNeedsRebuild eg)
  let assoc = egNodeAssoc eg
      hc = egHashCons eg
      bwd = assocBwd assoc
      rootNodes = ILS.fromList (assocRoots assoc)
      leafNodes = ILS.fromList (assocLeaves assoc)
      allNodes = ILS.union rootNodes leafNodes
      ef = egEquivFind eg
      rootClasses = ILS.fromList (efRoots ef)
      leafClasses = ILS.fromList (efLeaves ef)
      deadClasses = egDeadClasses eg
      cm = egClassMap eg
      cmClasses = ILS.fromList (ILM.keys cm)
  -- Assert that root nodes and leaf nodes are disjoint
  ILS.intersection rootNodes leafNodes === ILS.empty
  -- Assert that root classes and leaf classes are disjoint
  ILS.intersection rootClasses leafClasses === ILS.empty
  -- Assert that dead classes are exactly the leaf classes
  deadClasses === leafClasses
  -- Assert that the assoc is 1-1 etc
  assertAssocInvariants assoc
  -- Assert that the hashcons and assoc have equal key sets
  ILS.fromList (ILM.keys hc) === allNodes
  -- Assert that hashcons has exactly the same values as unionfind roots for all nodes
  for_ (ILM.elems hc) $ \c ->
    assert $ ILS.member c rootClasses
  -- Assert that classmap contains all unionfind roots
  for_ (ILS.toList rootClasses) $ \r ->
    assert $ ILS.member r cmClasses
  -- Assert that those non-root classes are marked dead
  for_ (ILS.toList cmClasses) $ \c ->
    unless (ILS.member c rootClasses) $
      assert $ ILS.member c deadClasses
  -- For every node, assert in the nodes of some class
  for_ (ILM.toList hc) $ \(n, c) -> do
    let nodes = eciNodes (ILM.partialLookup c cm)
    assert (ILS.member n nodes)
  -- For every root, assert is in all parent classes
  for_ (ILM.toList hc) $ \(n, c) ->
    when (ILS.member n rootNodes) $ do
      -- for all children that are not of the node's own class
      let children = ILS.filter (/= c) (foldMap ILS.singleton (assocPartialLookupByKey n assoc))
      -- traceM (unwords ["VERIFYING", show n, show c, show children])
      for_ (ILS.toList children) $ \y -> do
        -- look up child and assert in child's parents
        let parents = eciParents (ILM.partialLookup y cm)
        -- traceM (unwords ["->", show y, show parents])
        assert (ILS.member n parents)
  -- For every non-dead class
  (cmNodes, cmParents) <- flipFoldM (ILS.empty, ILS.empty) (ILM.toList cm) $ \(accNodes, accParents) (c, eci) ->
    if ILS.member c deadClasses
      -- skip dead classes
      then pure (accNodes, accParents)
      else do
        let nodes = eciNodes eci
            parents = eciParents eci
        -- Assert that classmap node values are non-empty
        nodes /== ILS.empty
        -- Assert that classmap class has node values that are hashconsed to class
        for_ (ILS.toList nodes) $ \n -> do
          ILM.lookup n hc === Just c
        -- Assert that classmap class has NO parents that are hashconsed to class
        -- dead nodes may have to be filtered on compact
        -- traceM (unwords ["CONSIDERING", show c, show parents])
        for_ (ILS.toList parents) $ \p ->
          ILM.lookup p hc /== Just c
        -- Assert we haven't seen these nodes before
        assert $ ILS.disjoint nodes accNodes
        -- Assert that the nodes and parents are disjoint
        assert $ ILS.disjoint nodes parents
        pure (ILS.union accNodes nodes, ILS.union accParents parents)
  let hcNodes = ILS.fromList (ILM.keys hc)
  -- Assert hc keys contain class nodes and dead nodes
  ILS.union cmNodes leafNodes === hcNodes
  -- Assert hc keys contain parent nodes
  assert $ ILS.isSubsetOf cmParents hcNodes
  -- Now test recanonicalization - we already know assoc fwd and bwd are 1-1
  for_ (HashMap.toList bwd) $ \(fc, _) ->
    let recanon = evalState (egCanonicalize fc) eg
    in recanon === Just fc

-- assert this after the usual eg invariants hold on both
assertEgCompactInvariants :: (MonadTest m, Eq d, Show d) => EGraph d f -> EGraph d f -> m ()
assertEgCompactInvariants egBefore egAfter = do
  let assocAfter = egNodeAssoc egAfter
      deadNodesAfter = ILS.fromList (assocLeaves assocAfter)
      deadClassesAfter = egDeadClasses egAfter
      aliveClassesAfter = ILS.fromList (ILM.keys (egClassMap egAfter))
      aliveNodesAfter = ILS.fromList (assocRoots assocAfter)
  -- First check that compact has removed dead nodes and classes
  -- dead classes should be empty
  deadClassesAfter === ILS.empty
  -- dead nodes should be empty
  deadNodesAfter === ILS.empty
  -- Now check equality of graphs
  let deadClassesBefore = egDeadClasses egBefore
      allClassesBefore = ILS.fromList (ILM.keys (egClassMap egBefore))
      aliveClassesBefore = ILS.difference allClassesBefore deadClassesBefore
      assocBefore = egNodeAssoc egBefore
      aliveNodesBefore = ILS.fromList (assocRoots assocBefore)
  -- that all non-dead classes and nodes are present in the compacted one
  aliveClassesBefore === aliveClassesAfter
  aliveNodesBefore === aliveNodesAfter
  -- for each alive class,
  for_ (ILS.toList aliveClassesBefore) $ \c -> do
    -- get the class info before and after
    let eciBefore = ILM.partialLookup c (egClassMap egBefore)
        eciAfter = ILM.partialLookup c (egClassMap egAfter)
        beforeNodes = eciNodes eciBefore
        remappedBeforeNodes = ILS.map (`assocLookupRoot` assocBefore) beforeNodes
        beforeParents = eciParents eciBefore
        remappedBeforeParents = ILS.map (`assocLookupRoot` assocBefore) beforeParents
    -- assert that class data is the same
    eciData eciAfter === eciData eciBefore
    -- assert that nodes are the same
    eciNodes eciAfter === remappedBeforeNodes
    -- assert that mappped parents are the same
    eciParents eciAfter === remappedBeforeParents

data EgRound = EgRound
  { egRoundTerms :: ![EGT]
  , egRoundSets :: ![[EGT]]
  , egRoundEqTests :: ![[EGT]]
  , egRoundNeqTests :: ![(EGT, EGT)]
  } deriving stock (Eq, Show)

data EgCase = EgCase
  { egCaseName :: !String
  , egCaseRounds :: ![EgRound]
  } deriving stock (Eq, Show)

allEgCases :: [EgCase]
allEgCases =
  let leafA = BinTreeLeaf (toV 'a')
      leafB = BinTreeLeaf (toV 'b')
      leafC = BinTreeLeaf (toV 'c')
      leafD = BinTreeLeaf (toV 'd')
      leafE = BinTreeLeaf (toV 'e')
      leafTerms = [leafA, leafB, leafC, leafD]
      parentAA = BinTreeBranch leafA leafA
      parentAB = BinTreeBranch leafA leafB
      parentAC = BinTreeBranch leafA leafC
      parentAD = BinTreeBranch leafA leafD
      parentBD = BinTreeBranch leafB leafD
      parentCA = BinTreeBranch leafC leafA
      simpleParentTerms = [parentAC, parentAD]
      complexParentTerms = [parentAC, parentBD]
      grandparentAAC = BinTreeBranch leafA parentAC
      grandparentAAD = BinTreeBranch leafA parentAD
      grandparentBAC = BinTreeBranch leafB parentAC
      grandparentEAD = BinTreeBranch leafE parentAD
      simpleGrandparentTerms = [grandparentAAC, grandparentAAD]
      complexGrandparentTerms = [grandparentBAC, grandparentEAD]
  in [ EgCase "simple"
          [ EgRound leafTerms [] [] [(leafA, leafB), (leafA, leafC), (leafB, leafC)]
          , EgRound [] [[leafA, leafB]] [[leafA, leafB]] [(leafA, leafC), (leafB, leafC)]
          ]
     , EgCase "transitive one round"
        [ EgRound leafTerms [] [] [(leafA, leafB), (leafA, leafC), (leafB, leafC), (leafA, leafD)]
        , EgRound [] [[leafA, leafB], [leafB, leafC]] [[leafA, leafB, leafC]] [(leafA, leafD)]
        ]
     , EgCase "transitive two round"
        [ EgRound leafTerms [] [] [(leafA, leafB), (leafA, leafC), (leafB, leafC), (leafA, leafD)]
        , EgRound [] [[leafA, leafB]] [[leafA, leafB]] [(leafA, leafC), (leafA, leafD)]
        , EgRound [] [[leafB, leafC]] [[leafA, leafB, leafC]] [(leafA, leafD)]
        ]
     , EgCase "simple parents"
        [ EgRound simpleParentTerms [] [] [(leafC, leafD), (parentAC, parentAD)]
        , EgRound [] [[leafC, leafD]] [[parentAC, parentAD]] []
        ]
     , EgCase "complex parents one round"
        [ EgRound complexParentTerms [] [] [(leafA, leafB), (leafC, leafD), (parentAC, parentBD)]
        , EgRound [] [[leafA, leafB], [leafC, leafD]] [[leafA, leafB], [leafC, leafD], [parentAC, parentBD]] []
        ]
     , EgCase "complex parents two round"
        [ EgRound complexParentTerms [] [] [(leafA, leafB), (leafC, leafD), (parentAC, parentBD)]
        , EgRound [] [[leafA, leafB]] [[leafA, leafB]] [(leafC, leafD), (parentAC, parentBD)]
        , EgRound [] [[leafC, leafD]] [[leafA, leafB], [leafC, leafD], [parentAC, parentBD]] []
        ]
     , EgCase "simple grandparents"
        [ EgRound simpleGrandparentTerms [] [] [(leafC, leafD), (parentAC, parentAD), (grandparentAAC, grandparentAAD)]
        , EgRound [] [[leafC, leafD]] [[leafC, leafD], [parentAC, parentAD], [grandparentAAC, grandparentAAD]] []
        ]
     , EgCase "complex grandparents bottom up"
        [ EgRound complexGrandparentTerms [] [] [(leafC, leafD), (leafB, leafE), (parentAC, parentAD), (grandparentBAC, grandparentEAD)]
        , EgRound [] [[leafC, leafD]] [[leafC, leafD], [parentAC, parentAD]] [(leafB, leafE), (grandparentBAC, grandparentEAD)]
        , EgRound [] [[leafB, leafE]] [[leafC, leafD], [leafB, leafE], [parentAC, parentAD], [grandparentBAC, grandparentEAD]] []
        ]
     , EgCase "complex grandparents top down"
        [ EgRound complexGrandparentTerms [] [] [(leafC, leafD), (leafB, leafE), (parentAC, parentAD), (grandparentBAC, grandparentEAD)]
        , EgRound [] [[leafB, leafE]] [[leafB, leafE]] [(leafC, leafD), (parentAC, parentAD), (grandparentBAC, grandparentEAD)]
        , EgRound [] [[leafC, leafD]] [[leafC, leafD], [leafB, leafE], [parentAC, parentAD], [grandparentBAC, grandparentEAD]] []
        ]
     , EgCase "connect"
        [ EgRound leafTerms [] [] [(leafA, leafB), (leafA, leafC), (leafB, leafC), (leafA, leafD)]
        , EgRound [] [[leafA, leafB]] [[leafA, leafB]] [(leafA, leafC), (leafA, leafD)]
        , EgRound [] [[leafC, leafD]] [[leafA, leafB], [leafC, leafD]] [(leafA, leafD)]
        , EgRound [] [[leafB, leafD]] [[leafA, leafB, leafC, leafD]] []
        ]
     , EgCase "mid grandparents"
        [ EgRound simpleGrandparentTerms [] [] [(leafC, leafD), (parentAC, parentAD), (grandparentAAC, grandparentAAD)]
        , EgRound [] [[parentAC, parentAD]] [[parentAC, parentAD], [grandparentAAC, grandparentAAD]] [(leafC, leafD)]
        , EgRound [] [[leafC, leafD]] [[leafC, leafD], [parentAC, parentAD], [grandparentAAC, grandparentAAD]] []
        ]
     , EgCase "unify node"
        [ EgRound [BinTreeBranch parentAC leafA, parentAA] [] [] [(parentAC, parentAA)]
        , EgRound [] [[leafA, leafC]] [[parentAC, parentAA]] []
        ]
     , EgCase "self parent"
        [ EgRound [BinTreeBranch parentAC leafB] [] [] []
        , EgRound [] [[parentAC, leafA]] [] []
        ]
     , EgCase "self parent again"
        [ EgRound [leafB, parentAA] [] [] []
        , EgRound [] [[leafB, leafA], [leafB, parentAA]] [] []
        ]
     , EgCase "dead add parent"
        [ EgRound [parentAC, leafB] [[leafA, parentAC], [leafA, leafC]] [[parentAC, leafC]] [(leafA, leafB)]
        , EgRound [parentCA] [] [[parentCA, parentAC]] []
        ]
     , EgCase "repro 1"
        [ EgRound [BinTreeBranch parentAA parentAB] [[leafB, leafA]] [] [(leafB, parentAA)]
        , EgRound [leafA] [[parentAA, leafA]] [[leafB, parentAA]] []
        ]
     ,  let grandparent = BinTreeBranch parentAA leafB
            greatGrandparent = BinTreeBranch grandparent leafA
        in EgCase "repro 2"
          [ EgRound [greatGrandparent, leafA, leafC] [[greatGrandparent, parentAA], [leafA, leafB]] [] []
          , EgRound [leafA, leafA] [[leafA, grandparent]] [] []
          , EgRound [leafA, leafA, leafA] [[parentAA, leafA]] [] []
          ]
     ]

testEgCase :: Bool -> EgCase -> TestTree
testEgCase compact (EgCase name rounds) = kase where
  findMayTerm t = fmap (egFindTerm t) get
  findTerm t = fmap fromJust (findMayTerm t)
  findTerms ts = fmap ILS.fromList (for ts findTerm)
  assertTermFound t = findMayTerm t >>= \mi -> assert (isJust mi)
  assertTermsFound ts = for_ ts assertTermFound
  kase = testUnit (name ++ " (" ++ (if compact then "compact" else "non-compact") ++ ")") $ runS egNew $ do
    -- for each round
    for_ rounds $ \(EgRound start act endEq endNeq) -> do
      -- add initial terms and assert invariants hold
      applyS (for_ start (egAddTerm maxVAnalysis))
      -- liftIO (putStrLn "===== post add =====")
      -- testS $ liftIO . pPrint
      testS assertEgInvariants
      -- assert that all mentioned terms are in the egraph
      assertTermsFound start
      for_ act assertTermsFound
      for_ endEq assertTermsFound
      for_ endNeq $ \(x, y) -> do
        -- also assert that neq terms are not themselves equal
        x /== y
        assertTermFound x
        assertTermFound y
      -- merge sets of terms and rebuild
      applyS $ do
        sets <- for act findTerms
        for_ sets egMergeMany
      -- liftIO (putStrLn "===== before rebuild =====")
      -- testS $ liftIO . pPrint
      _ <- applyS (egRebuild maxVAnalysis)
      -- liftIO (putStrLn "===== after rebuild =====")
      -- testS $ liftIO . pPrint
      -- assert invariants hold
      testS assertEgInvariants
      -- find merged terms again and assert they are in same classes
      sets <- applyS $ for act findTerms
      for_ sets $ \set -> ILS.size set === 1
      -- find final eq terms and assert they are in same classes
      for_ endEq $ \ts -> do
        set <- applyS (findTerms ts)
        ILS.size set === 1
      -- find final neq terms and assert they are not in same class
      for_ endNeq $ \(x, y) -> do
        i <- applyS (findTerm x)
        j <- applyS (findTerm y)
        i /== j
      -- compact if configured to do so
      when compact $ do
        -- liftIO (putStrLn "===== before compact =====")
        -- testS $ liftIO . pPrint
        egBefore <- get
        applyS egCompact
        -- liftIO (putStrLn "===== after compact =====")
        -- testS $ liftIO . pPrint
        testS assertEgInvariants
        testS (assertEgCompactInvariants egBefore)

testEgCases :: TestTree
testEgCases = testGroup "Eg case" $ do
  kase <- allEgCases
  compact <- [False, True]
  pure (testEgCase compact kase)

testEgNew :: TestTree
testEgNew = testUnit "EG new" $ do
  eg0 <- fullyEvaluate (egNew :: EGV)
  egNodeSize eg0 === 0
  egClassSize eg0 === 0
  assertEgInvariants eg0

genNodePairs :: Range Int -> EGV -> Gen [(EClassId, EClassId)]
genNodePairs nOpsRange eg = genListOfDistinctPairs nOpsRange (ILM.keys (egClassMap eg))

genSomeList :: [a] -> Gen [a]
genSomeList xs = go where
  go = Gen.recursive Gen.choice [Gen.constant [], fmap pure (Gen.element xs)] [Gen.subterm2 go go (++)]

genBinTree :: Gen a -> Gen (BinTree a)
genBinTree genA = genEither where
  genLeaf = fmap BinTreeLeaf genA
  genBranch = Gen.subterm2 genEither genEither BinTreeBranch
  genEither = Gen.recursive Gen.choice [genLeaf] [genBranch]

genBinTreeMembers :: Int -> Gen [BinTree V]
genBinTreeMembers maxElems = Gen.list (Range.linear 0 maxElems) (genBinTree (genV maxElems))

-- An alternative to 'genBinTreeMembers' that makes smaller trees
mkSimpleTreeLevels :: Int -> [BinTree V]
mkSimpleTreeLevels maxElems =
  let letters = take maxElems (['a'..'z'] ++ ['A'..'Z'])
      zeroLevel = fmap (BinTreeLeaf . toV) letters
      mkLevel y x = (BinTreeBranch <$> x <*> y) ++ (BinTreeBranch <$> y <*> x)
      mkLevels y xs = foldr (\x r -> mkLevel y x ++ r) (BinTreeBranch <$> y <*> y) xs
      oneLevel = mkLevels zeroLevel []
      twoLevel = mkLevels oneLevel [zeroLevel]
      anyLevel = zeroLevel ++ oneLevel ++ twoLevel
  in anyLevel

testEgProp :: TestLimit -> TestTree
testEgProp lim = after AllSucceed "EG unit" $ after AllSucceed "EG cases" $ testGen "EG prop" lim prop where
  maxElems = 10
  termGen = genBinTreeMembers maxElems
  -- Guarantee yourself small trees with this:
  -- termGen = genSomeList (mkSimpleTreeLevels maxElems)
  prop = do
    eg0 <- fullyEvaluate (egNew :: EGV)
    rounds <- forAll (Gen.element [1, 2, 3])
    body rounds eg0
  body (rounds :: Int) eg0 = do
    members <- forAll termGen
    let nMembers = length members
        nOpsRange = Range.linear 0 (nMembers * nMembers)
    eg1 <- fullyEvaluate (execState (for_ members (egAddTerm maxVAnalysis)) eg0)
    assertEgInvariants eg1
    execState (egRebuild maxVAnalysis) eg1 === eg1
    pairs <- forAll (genNodePairs nOpsRange eg1)
    let merge = for_ pairs (uncurry egMerge)
    eg2 <- fullyEvaluate (execState merge eg1)
    -- liftIO (putStrLn "===== eg2 =====")
    -- liftIO (pPrint eg2)
    egNodeSize eg2 === egNodeSize eg1
    eg3 <- fullyEvaluate (execState (egRebuild maxVAnalysis) eg2)
    -- liftIO (putStrLn "===== eg3 =====")
    -- liftIO (pPrint eg3)
    egNodeSize eg3 === egNodeSize eg2
    assertEgInvariants eg3
    compact <- forAll Gen.bool
    eg4 <- if compact
      then do
        egX <- fullyEvaluate (execState egCompact eg3)
        assertEgInvariants egX
        assertEgCompactInvariants eg3 egX
        pure egX
      else pure eg3
    unless (rounds == 1) (body (rounds - 1) eg4)

type M = IntLikeMap ENodeId Char

testILM :: TestTree
testILM = testUnit "ILM unit" $ do
  let mLeft = ILM.fromList [(ENodeId 0, 'a'), (ENodeId 1, 'b')] :: M
      mRight = ILM.fromList [(ENodeId 1, 'x'), (ENodeId 2, 'c')] :: M
      mMerged = ILM.fromList [(ENodeId 0, 'a'), (ENodeId 1, 'b'), (ENodeId 2, 'c')] :: M
  mLeft <> mRight === mMerged

main :: IO ()
main = do
  lim <- setupTests
  defaultMain $ testGroup "Overeasy"
    -- [ testEgCase True (allEgCases !! (length allEgCases - 1)) ]
    -- [ testEgCase t (fromJust (find (\(EgCase n _) -> n == "repro 2") allEgCases)) | t <- [True]]
    -- [ testEgProp lim ]
    [ testILM
    , testUfUnit
    , testAssocUnit
    , testAssocCases
    , testEgUnit
    , testEgNew
    , testEgCases
    , testUfProp lim
    , testEgProp lim
    ]
