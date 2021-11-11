module Overeasy.Test.Spec (main) where

import Control.Monad (foldM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState (..), State, StateT, evalState, evalStateT, execState, execStateT,
                                   runState, gets)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (bimap)
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.Functor.Foldable (cata)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import Data.Semigroup (Max (..))
import qualified Data.Sequence as Seq
import Hedgehog (Gen, PropertyT, Range, assert, forAll, property, (/==), (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Overeasy.Assoc (Assoc, assocAdd, assocBwd, assocCanCompact, assocCompact, assocDeadBwd, assocDeadFwd,
                       assocFromPairs, assocFwd, assocNew, assocSize, assocSrc, assocUpdate)
import Overeasy.Classes (Changed (..))
import Overeasy.EGraph (EAnalysisAlgebra (..), EAnalysisOff (..), EClassId (..), EClassInfo (..), EGraph (..),
                        ENodeId (..), egAddTerm, egCanonicalize, egClassSize, egFindTerm, egMerge, egNeedsRebuild,
                        egNew, egNodeSize, egRebuild, egTotalClassSize, egWorkList)
import Overeasy.Expressions.BinTree (BinTree, BinTreeF (..), pattern BinTreeBranch, pattern BinTreeLeaf)
import qualified Overeasy.IntLike.Equiv as ILE
import qualified Overeasy.IntLike.Graph as ILG
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Overeasy.Source (sourcePeek)
import Overeasy.Test.Arith (ArithF, pattern ArithConst, pattern ArithPlus)
import Overeasy.Test.Assertions (assertFalse, assertTrue, (@/=))
import Overeasy.EquivFind (EquivMergeRes (..), EquivFind (..), efAdd, efFind, efElems, efMerge, efMergeMany, efNew,
                           efRoots, efSize, efTotalSize)
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Test.Tasty (DependencyType (..), TestTree, after, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Text.Pretty.Simple (pPrint)
import Control.DeepSeq (force, NFData)
import Overeasy.UnionFind (ufRoots)

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

type UF = EquivFind V

runUF :: StateT UF IO () -> IO ()
runUF = runS efNew

testUfSimple :: TestTree
testUfSimple = testCase "UF simple" $ runUF $ do
  testS $ \ef -> do
    efSize ef @?= 0
    efTotalSize ef @?= 0
    efRoots ef @?= []
  applyS (efAdd (toV 'a'))
  testS $ \ef -> do
    efSize ef @?= 1
    efTotalSize ef @?= 1
    ILS.fromList (efRoots ef) @?= setV "a"
  applyS (efAdd (toV 'b'))
  applyS (efAdd (toV 'c'))
  testS $ \ef -> do
    efSize ef @?= 3
    efTotalSize ef @?= 3
    ILS.fromList (efRoots ef) @?= setV "abc"
  applyTestS (efMerge (toV 'a') (toV 'c')) $ \res ef -> do
    res @?= EquivMergeResChanged (toV 'a')
    efSize ef @?= 2
    efTotalSize ef @?= 3
    ILS.fromList (efRoots ef) @?= setV "ab"
    efFwd ef @?= ILM.fromList [(toV 'a', setV "ac"), (toV 'b', setV "b")]
  applyTestS (efMerge (toV 'c') (toV 'a')) $ \res _ -> res @?= EquivMergeResUnchanged (toV 'a')
  applyTestS (efMerge (toV 'b') (toV 'z')) $ \res _ -> res @?= EquivMergeResMissing (toV 'z')

testUfRec :: TestTree
testUfRec = testCase "UF rec" $ runUF $ do
  applyS (efAdd (toV 'a'))
  applyS (efAdd (toV 'b'))
  applyS (efAdd (toV 'c'))
  applyS_ (efMerge (toV 'b') (toV 'c'))
  applyS_ (efMerge (toV 'a') (toV 'c'))
  testS $ \ef -> do
    efSize ef @?= 1
    efTotalSize ef @?= 3
    ILS.fromList (efRoots ef) @?= setV "a"
    efFwd ef @?= ILM.fromList [(toV 'a', setV "abc")]

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
  in fmap V (Gen.int (Range.constant minVal maxVal))

genMembers :: Int -> Gen [V]
genMembers maxElems = do
  let nElemsRange = Range.linear 0 maxElems
      minVal = ord 'a'
  n <- Gen.int nElemsRange
  pure (fmap (\i -> V (minVal + i)) [0..n-1])

mkInitUf :: [V] -> UF
mkInitUf vs = execState (for_ vs efAdd) efNew

mkSingleMergedUf :: [(V, V)] -> UF -> UF
mkSingleMergedUf vvs = execState (for_ vvs (uncurry efMerge))

mkMultiMergedUf :: [(V, V)] -> UF -> UF
mkMultiMergedUf vvs = execState (for_ vvs (\(x, y) -> efMergeMany (ILS.fromList [x, y])))

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
    efSize initUf === nMembers
    efTotalSize initUf === nMembers
    -- assert that find indicates nothing is merged
    for_ allPairs $ \(a, b) -> flip evalStateT initUf $ do
      x <- applyS (gets (efFind a))
      y <- applyS (gets (efFind b))
      lift (x /== y)
    -- generate some pairs and merge them
    mergePairs <- forAll (genListOfDistinctPairs nOpsRange memberList)
    shouldMultiMerge <- forAll Gen.bool_
    let mergedUf =
          if shouldMultiMerge
            then mkMultiMergedUf mergePairs initUf
            else mkSingleMergedUf mergePairs initUf
    -- assert that total size is unchanged
    efTotalSize mergedUf === nMembers
    -- calculate components by graph reachability
    let components = ILG.undirectedComponents mergePairs
    -- assert that elements are equal or not according to component
    void $ foldS_ mergedUf allPairs $ \(a, b) -> do
      x <- applyS (gets (efFind a))
      y <- applyS (gets (efFind b))
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

-- | Asserts assoc is compact - should also check 'assertAssocInvariants'
assertAssocCompact :: (Eq a, Hashable a, Show a) => Assoc ENodeId a -> Assertion
assertAssocCompact av = do
  let fwd = assocFwd av
      bwd = assocBwd av
  -- Assert that the assoc has been rebuilt
  assertFalse (assocCanCompact av)
  assocDeadFwd av @?= ILS.empty
  assocDeadBwd av @?= HashSet.empty
  -- Look at sizes to confirm that assoc could map 1-1
  ILM.size fwd @?= HashMap.size bwd
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(x, fc) -> do
    -- Assert is found in backward map AND maps back
    HashMap.lookup fc bwd @?= Just x
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(fc, x) ->
    -- Assert is present in forward map AND maps back
    ILM.lookup x fwd @?= Just fc

-- | Asserts assoc is correctly structured (compact or not)
assertAssocInvariants :: (Eq a, Hashable a) => Assoc ENodeId a -> Assertion
assertAssocInvariants av = do
  let fwd = assocFwd av
      bwd = assocBwd av
  let nextId = unENodeId (sourcePeek (assocSrc av))
  -- Go through keys forward
  for_ (ILM.toList fwd) $ \(x, fc) -> do
    -- Assert is found in backward map
    assertTrue (HashMap.member fc bwd)
    -- Assert is less than next fresh id
    assertTrue (nextId > unENodeId x)
  -- Go through keys backward
  for_ (HashMap.toList bwd) $ \(_, x) ->
    -- Assert is present in forward map
    assertTrue (ILM.member x fwd)

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

runAV :: [(Int, Char)] -> StateT AV IO () -> IO ()
runAV start act = do
  aStart <- mkAssoc start
  runS aStart act

testAssocCase :: AssocCase -> TestTree
testAssocCase (AssocCase name start act end) = testCase name $ runAV start $ do
  testS $ \av -> do
    assertAssocInvariants av
    assertAssocCompact av
    assocSize av @?= length start
  for_ act $ \(x, a, y) -> do
    z <- applyS (assocUpdate (ENodeId x) (toV a))
    testS assertAssocInvariants
    liftIO (z @?= ENodeId y)
  applyS assocCompact
  testS $ \av -> do
    assertAssocInvariants av
    assertAssocCompact av
    assocSize av @?= length end
    endAv <- liftIO (mkAssoc end)
    assocFwd av @?= assocFwd endAv
    assocBwd av @?= assocBwd endAv

testAssocCases :: TestTree
testAssocCases = testGroup "Assoc case" (fmap testAssocCase allAssocCases)

testAssocUnit :: TestTree
testAssocUnit = testCase "Assoc unit" $ do
  let a0 = assocNew (ENodeId 0) :: AV
  assertAssocInvariants a0
  assertAssocCompact a0
  assocSize a0 @?= 0
  let members = [toV 'a', toV 'b', toV 'c'] :: [V]
  let a1 = execState (for_ members assocAdd) a0
  assertAssocInvariants a1
  assertAssocCompact a0
  assocSize a1 @?= 3
  let aVal = toV 'a'
      aKey = fromJust (HashMap.lookup aVal (assocBwd a1))
      bVal = toV 'b'
      bKey = fromJust (HashMap.lookup bVal (assocBwd a1))
  let (newAKey, a2) = runState (assocUpdate aKey bVal) a1
  newAKey @?= bKey
  assertAssocInvariants a2
  assertTrue (assocCanCompact a2)
  assocDeadFwd a2 @?= ILS.fromList [aKey]
  assocDeadBwd a2 @?= HashSet.fromList [aVal]
  let a3 = execState assocCompact a2
  assertAssocInvariants a3
  assertAssocCompact a3
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
  applyTestS (egMerge cidFour cidFour) $ \m eg -> do
    egNeedsRebuild eg @?= False
    case m of
      Nothing -> fail "Could not resolve cidFour"
      Just c -> c @?= ChangedNo
  -- Merge `2 + 2` and `4`
  applyTestS (egMerge cidPlus cidFour) $ \m eg -> do
    egNeedsRebuild eg @?= True
    egWorkList eg @?= Seq.singleton (ILS.fromList [cidPlus, cidFour])
    case m of
      Nothing -> fail "Could not resolve one of cidFour or cidPlus"
      Just c -> c @?= ChangedYes
  -- Now rebuild
  applyTestS (egRebuild noA) $ \newRoots eg -> do
    cidMerged <-
      case ILS.toList newRoots of
        [x] -> pure x
        _ -> fail "Expected singleton root list"
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
genBinTreeMembers maxElems = Gen.list (Range.constant 0 maxElems) (genBinTree (genV maxElems))

type EGD = Max V
type EGF = BinTreeF V
type EGT = BinTree V
type EGV = EGraph EGD EGF

maxVAnalysis :: EAnalysisAlgebra EGD EGF
maxVAnalysis = EAnalysisAlgebra $ \case
  BinTreeLeafF v -> Max v
  BinTreeBranchF d1 d2 -> d1 <> d2

analyzeBinTree :: Semigroup m => (a -> m) -> BinTree a -> m
analyzeBinTree f = cata go where
  go = \case
    BinTreeLeafF a -> f a
    BinTreeBranchF x y -> x <> y

maxBinTreeLeaf :: Ord a => BinTree a -> a
maxBinTreeLeaf = getMax . analyzeBinTree Max

propEgInvariants :: (Traversable f, Eq (f EClassId), Hashable (f EClassId), Show (f EClassId)) => EGraph d f -> PropertyT IO ()
propEgInvariants eg = do
  -- Invariants require that no rebuild is needed (empty worklist)
  assert (not (egNeedsRebuild eg))
  -- First look at the assoc (NodeId <-> f ClassId)
  let assoc = egNodeAssoc eg
      fwd = assocFwd assoc
      bwd = assocBwd assoc
      deadFwd = assocDeadFwd assoc
      deadBwd = assocDeadBwd assoc
  -- Assert that the assoc is 1-1 etc
  liftIO (assertAssocInvariants assoc)
  -- Now look at hashcons (NodeId -> ClassId)
  let hc = egHashCons eg
  -- Assert that the hashcons and assoc have equal key sets
  ILM.keys hc === ILM.keys fwd
  -- Assert that hashcons has exactly the same values as unionfind roots for non-dead nodes
  let ef = egEquivFind eg
      efRootClasses = ILS.fromList (efRoots ef)
  for_ (ILM.toList hc) $ \(n, c) ->
    unless (ILS.member n deadFwd) $ do
      assert (ILS.member c efRootClasses)
  -- Assert that classmap has exactly the same keys as unionfind roots
  let cm = egClassMap eg
      cmClasses = ILS.fromList (ILM.keys cm)
  cmClasses === efRootClasses
  cmNodes <- flipFoldM ILS.empty (ILM.toList cm) $ \accNodes (c, eci) -> do
    let nodes = eciNodes eci
    -- Assert that classmap node values are non-empty
    nodes /== ILS.empty
    -- Assert that classmap class has node values that are hashconsed to class
    for_ (ILS.toList nodes) $ \n ->
      ILM.lookup n hc === Just c
    assert (ILS.disjoint nodes accNodes)
    pure (accNodes <> nodes)
  -- Assert that all node values in all classmap classes equal hc keys
  cmNodes === ILS.fromList (ILM.keys hc)
  -- Now test recanonicalization for non-dead nodes
  for_ (HashMap.toList bwd) $ \(fc, n) ->
    unless (HashSet.member fc deadBwd) $
      let recanon = evalState (egCanonicalize fc) eg
      in do
        liftIO (pPrint n)
        liftIO (pPrint fc)
        liftIO (pPrint recanon)
        recanon === Just fc

genNodePairs :: Range Int -> EGV -> Gen [(EClassId, EClassId)]
genNodePairs nOpsRange eg = genListOfDistinctPairs nOpsRange (ILM.keys (egClassMap eg))

testEgProp :: TestTree
testEgProp = after AllSucceed "EG unit" $ testProperty "EG prop" $
  let maxElems = 5
      eg0 = force egNew :: EGV
  in property $ do
    -- liftIO (putStrLn "===== eg0 =====")
    -- liftIO (pPrint eg0)
    assert (egNodeSize eg0 == 0)
    assert (egClassSize eg0 == 0)
    propEgInvariants eg0
    -- XXX add forAlls back
    -- members <- forAll (genBinTreeMembers maxElems)
    -- let members = [BinTreeLeaf (toV 'a'), BinTreeLeaf (toV 'b'), BinTreeLeaf (toV 'c')] :: [EGT]
    -- let members = [BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeLeaf (toV 'a')))]
    -- let members = [BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeLeaf (toV 'b'))]
    -- let members = [BinTreeLeaf (toV 'a'), BinTreeBranch (BinTreeLeaf (toV 'b')) (BinTreeLeaf (toV 'c'))]
    -- let members = [ BinTreeLeaf (toV 'a') , BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeLeaf (toV 'a')) , BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeLeaf (toV 'b')) ]
    -- let members = [BinTreeLeaf (toV 'a'), BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeBranch (BinTreeLeaf (toV 'a')) (BinTreeLeaf (toV 'a')))]
    let members = [ BinTreeBranch
                        (BinTreeBranch
                           (BinTreeBranch (BinTreeLeaf (toV 'l')) (BinTreeLeaf (toV 'j')))
                           (BinTreeLeaf (toV 'b')))
                        (BinTreeLeaf (toV 'j'))
                    , BinTreeLeaf (toV 'a')
                    , BinTreeBranch
                        (BinTreeLeaf (toV 'a'))
                        (BinTreeBranch (BinTreeLeaf (toV 'b')) (BinTreeLeaf (toV 'a')))
                  ]
    let nMembers = length members
        nOpsRange = Range.linear 0 (nMembers * nMembers)
    let eg1 = force (execState (for_ members (egAddTerm maxVAnalysis)) eg0)
    -- liftIO (putStrLn "===== eg1 =====")
    -- liftIO (pPrint eg1)
    propEgInvariants eg1
    assert (egNodeSize eg1 >= 0)
    egClassSize eg1 === egNodeSize eg1
    execState (egRebuild maxVAnalysis) eg1 === eg1
    -- pairs <- forAll (genNodePairs nOpsRange eg1)
    -- let pairs = [(EClassId 0, EClassId 1)]
    -- let pairs = [(EClassId 1, EClassId 2), (EClassId 0, EClassId 1)]
    -- let pairs = [(EClassId 0, EClassId 2), (EClassId 0, EClassId 1)]
    -- let pairs = [(EClassId 0, EClassId 1), (EClassId 0, EClassId 2)]
    -- let pairs = [(EClassId 0, EClassId 3), (EClassId 0, EClassId 1)]
    let pairs = [(EClassId 4, EClassId 0), (EClassId 5, EClassId 6), (EClassId 0, EClassId 1)]
    let eg2 = force (execState (for_ pairs (uncurry egMerge)) eg1)
    liftIO (putStrLn "===== eg2 =====")
    liftIO (pPrint eg2)
    egNodeSize eg2 === egNodeSize eg1
    egNeedsRebuild eg2 === not (null pairs)
    let eg3 = force (execState (egRebuild maxVAnalysis) eg2)
    liftIO (putStrLn "===== eg3 =====")
    liftIO (pPrint eg3)
    egNodeSize eg3 === egNodeSize eg2
    propEgInvariants eg3

type M = IntLikeMap ENodeId Char

testILM :: TestTree
testILM = testCase "ILM unit" $ do
  let mLeft = ILM.fromList [(ENodeId 0, 'a'), (ENodeId 1, 'b')] :: M
      mRight = ILM.fromList [(ENodeId 1, 'x'), (ENodeId 2, 'c')] :: M
      mMerged = ILM.fromList [(ENodeId 0, 'a'), (ENodeId 1, 'b'), (ENodeId 2, 'c')] :: M
  mLeft <> mRight @?= mMerged

main :: IO ()
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  defaultMain $ testGroup "Overeasy"
    [ testILM
    , testUfUnit
    -- , testEgUnit
    , testAssocCases
    , testAssocUnit
    , testUfProp
    -- , testEgProp
    ]
