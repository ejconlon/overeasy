{-# LANGUAGE DeriveAnyClass #-}

-- | A Union-Find implementation using explicit equivalence classes.
-- We inevitably have to construct these classes so we might as well just do it as we go!
module Overeasy.EquivFind
  ( EquivFind
  , efFwd
  , efBwd
  , efRootsSize
  , efLeavesSize
  , efTotalSize
  , efNew
  , efMember
  , efRoots
  , efLeaves
  , efAddInc
  , efAdd
  , efEquivs
  , efClosure
  , efFindRoot
  , efFindLeaves
  , efLookupRoot
  , efLookupLeaves
  , efFindAll
  , EquivMergeRes (..)
  , efMergeInc
  , efMerge
  , EquivMergeSetsRes (..)
  , efMergeSetsInc
  , efMergeSets
  , efCompactInc
  , efCompact
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Control.Applicative ((<|>))

-- private ctor
data EquivFind x = EquivFind
  { efFwd :: !(IntLikeMap x (IntLikeSet x))
  , efBwd :: !(IntLikeMap x x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

efRootsSize :: EquivFind x -> Int
efRootsSize = ILM.size . efFwd

efLeavesSize :: EquivFind x -> Int
efLeavesSize = ILM.size . efBwd

efTotalSize :: EquivFind x -> Int
efTotalSize ef = efRootsSize ef + efLeavesSize ef

-- | Creates a new UF
efNew :: EquivFind x
efNew = EquivFind ILM.empty ILM.empty

-- private
allocMM :: Coercible x Int => x -> IntLikeMap x (IntLikeSet x) -> IntLikeMap x (IntLikeSet x)
allocMM = ILM.alter (<|> Just ILS.empty)

insertMM :: Coercible x Int => x -> x -> IntLikeMap x (IntLikeSet x) -> IntLikeMap x (IntLikeSet x)
insertMM x y = ILM.alter (\case { Nothing -> Just (ILS.singleton y); Just s -> Just (ILS.insert y s) }) x

efAddInc :: Coercible x Int => x -> EquivFind x -> (x, EquivFind x)
efAddInc x u@(EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing ->
      if ILM.member x fwd
        then (x, u)
        else (x, EquivFind (ILM.insert x ILS.empty fwd) bwd)
    Just y -> (y, u)

efAdd :: Coercible x Int => x -> State (EquivFind x) x
efAdd = state . efAddInc

efEquivs :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efEquivs x (EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing -> ILS.empty
    Just y -> ILM.partialLookup y fwd

efClosure :: Coercible x Int => [x] -> EquivFind x -> IntLikeSet x
efClosure xs ef = foldl' (\c x -> if ILS.member x c then c else ILS.union (efEquivs x ef) c) ILS.empty xs

-- -- | For all given classes, construct a map of class root to all class elems (not including the root)
-- efRootMap :: Coercible x Int => [x] -> EquivFind x -> IntLikeMultiMap x x
-- efRootMap xs (EquivFind fwd bwd) = foldl' go ILM.empty xs where
--   go m x =
--     case ILM.lookup x bwd of
--       Nothing -> m
--       Just r ->
--         case ILM.lookup r m of
--           Nothing -> ILM.insert r (ILM.partialLookup r fwd) m
--           _ -> m

efFindRoot :: Coercible x Int => x -> EquivFind x -> Maybe x
efFindRoot x ef = ILM.lookup x (efBwd ef) <|> if ILM.member x (efFwd ef) then Just x else Nothing

efFindLeaves :: Coercible x Int => x -> EquivFind x -> Maybe (IntLikeSet x)
efFindLeaves x ef = ILM.lookup x (efFwd ef)

-- | Like 'efFindRoot' but returns same key if not found - does not guarantee presence in map
efLookupRoot :: Coercible x Int => x -> EquivFind x -> x
efLookupRoot x = fromMaybe x . ILM.lookup x . efBwd

-- | Like 'efFindLEaves' but returns empty set if not found - does not guarantee presence in map
efLookupLeaves :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efLookupLeaves x = fromMaybe ILS.empty . ILM.lookup x . efFwd

efFindAll :: Coercible x Int => [x] -> EquivFind x -> Either x (IntLikeSet x)
efFindAll xs ef = go ILS.empty xs where
  go !acc = \case
    [] -> Right acc
    y:ys ->
      case efFindRoot y ef of
        Nothing -> Left y
        Just z -> go (ILS.insert z acc) ys

efMember :: Coercible x Int => x -> EquivFind x -> Bool
efMember x  = ILM.member x . efBwd

efRoots :: Coercible x Int => EquivFind x -> [x]
efRoots = ILM.keys . efFwd

efLeaves :: Coercible x Int => EquivFind x -> [x]
efLeaves = ILM.keys . efBwd

-- | The result of trying to merge two elements of the 'EquivFind'
data EquivMergeRes x =
    EquivMergeResMissing !x
  | EquivMergeResUnchanged !x
  | EquivMergeResChanged !x !(IntLikeSet x) !(EquivFind x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

efMergeInc :: (Coercible x Int, Ord x) => x -> x -> EquivFind x -> EquivMergeRes x
efMergeInc i j ef@(EquivFind fwd bwd) =
  case efFindRoot i ef of
    Nothing -> EquivMergeResMissing i
    Just ix ->
      case efFindRoot j ef of
        Nothing -> EquivMergeResMissing j
        Just jx ->
          if ix == jx
            then EquivMergeResUnchanged ix
            else
              let loKey = min ix jx
                  hiKey = max ix jx
                  hiSet = ILS.insert hiKey (ILM.partialLookup hiKey fwd)
                  finalFwd = ILM.adjust (hiSet <>) loKey (ILM.delete hiKey fwd)
                  finalBwd = foldl' (flip (`ILM.insert` loKey)) bwd (ILS.toList hiSet)
              in EquivMergeResChanged loKey hiSet (EquivFind finalFwd finalBwd)

efMerge :: (Coercible x Int, Ord x) => x -> x -> State (EquivFind x) (Maybe (x, IntLikeSet x))
efMerge i j = state $ \ef ->
  case efMergeInc i j ef of
    EquivMergeResChanged loKey hiSet ef' -> (Just (loKey, hiSet), ef')
    _ -> (Nothing, ef)

-- | The result of trying to merge multiple elements of the 'EquivFind'
data EquivMergeManyRes x =
    EquivMergeManyResEmpty
  | EquivMergeManyResEmbed !(EquivMergeRes x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data EquivMergeSetsRes x =
    EquivMergeSetsResEmptySet
  | EquivMergeSetsResMissing !x
  | EquivMergeSetsResUnchanged !(IntLikeSet x)
  | EquivMergeSetsResChanged !(IntLikeSet x) !(IntLikeSet x) !(EquivFind x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

efMergeSetsInc :: Coercible x Int => [IntLikeSet x] -> EquivFind x -> EquivMergeSetsRes x
efMergeSetsInc css0 u0 = res where
  res =
    case css0 of
      [] -> EquivMergeSetsResUnchanged ILS.empty
      _ -> go ILS.empty ILS.empty u0 css0
  go !roots !classRemapSet u@(EquivFind fwd bwd) css =
    case css of
      [] ->
        let finalRoots = ILS.map (`efLookupRoot` u) roots
        in if ILS.null classRemapSet
          then EquivMergeSetsResUnchanged finalRoots
          else EquivMergeSetsResChanged finalRoots classRemapSet u
      ds:dss ->
        case ILS.toList ds of
          [] -> go roots classRemapSet u dss
          zs -> case efFindAll zs u of
            Left x -> EquivMergeSetsResMissing x
            Right xs ->
              let (loKey, ys) = fromJust (ILS.minView xs)
                  newRoots = ILS.insert loKey roots
                  hiSet = ILS.unions (fmap (\y -> ILS.insert y (efLookupLeaves y u)) (ILS.toList ys))
                  newClassRemapSet = ILS.union hiSet classRemapSet
                  newFwd = ILM.adjust (ILS.union hiSet) loKey (foldl' (flip ILM.delete) fwd (ILS.toList ys))
                  newBwd = foldl' (flip (`ILM.insert` loKey)) bwd (ILS.toList hiSet)
                  newU = EquivFind newFwd newBwd
              in go newRoots newClassRemapSet newU dss

efMergeSets :: Coercible x Int => [IntLikeSet x] -> State (EquivFind x) (Maybe (IntLikeSet x, IntLikeSet x))
efMergeSets css = state $ \ef ->
  case efMergeSetsInc css ef of
    EquivMergeSetsResChanged roots classRemapSet ef' -> (Just (roots, classRemapSet), ef')
    _ -> (Nothing, ef)

efCompactInc :: Coercible x Int => IntLikeSet x -> EquivFind x -> (IntLikeSet x, EquivFind x)
efCompactInc dc u@(EquivFind fwd bwd) =
  let roots = ILS.map (`efLookupRoot` u) dc
      fwd' = foldl' (flip (ILM.adjust (`ILS.difference` dc))) fwd (ILS.toList roots)
      bwd' = foldl' (flip ILM.delete) bwd (ILS.toList dc)
  in (roots, EquivFind fwd' bwd')

efCompact :: Coercible x Int => IntLikeSet x -> State (EquivFind x) (IntLikeSet x)
efCompact = state . efCompactInc
