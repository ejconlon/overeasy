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
  , EquivAddRes (..)
  , efAddInc
  , efAdd
  , efEquivs
  , efClosure
  , efFindRoot
  , efFindLeaves
  , efSubset
  , efLookupRoot
  , efLookupLeaves
  , efFindAll
  , EquivMergeRes (..)
  , efUnsafeMerge
  , efMergeInc
  , efMerge
  , EquivMergeSetsRes (..)
  , efMergeSetsInc
  , efMergeSets
  , efCanCompact
  , efCompactInc
  , efCompact
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS

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

-- private
insertMM :: Coercible x Int => x -> x -> IntLikeMap x (IntLikeSet x) -> IntLikeMap x (IntLikeSet x)
insertMM x y = ILM.alter (\case { Nothing -> Just (ILS.singleton y); Just s -> Just (ILS.insert y s) }) x

data EquivAddRes x =
    EquivAddResAlreadyRoot
  | EquivAddResAlreadyLeafOf !x
  | EquivAddResNewRoot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

efAddInc :: Coercible x Int => x -> EquivFind x -> (EquivAddRes x, EquivFind x)
efAddInc x ef@(EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing ->
      if ILM.member x fwd
        then (EquivAddResAlreadyRoot, ef)
        else (EquivAddResNewRoot, EquivFind (ILM.insert x ILS.empty fwd) bwd)
    Just y -> (EquivAddResAlreadyLeafOf y, ef)

efAdd :: Coercible x Int => x -> State (EquivFind x) (EquivAddRes x)
efAdd = state . efAddInc

efEquivs :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efEquivs x ef = let r = efLookupRoot x ef in ILS.insert r (efLookupLeaves r ef)

efClosure :: Coercible x Int => [x] -> EquivFind x -> IntLikeSet x
efClosure xs ef = foldl' (\c x -> if ILS.member x c then c else ILS.union (efEquivs x ef) c) ILS.empty xs

efFindRoot :: Coercible x Int => x -> EquivFind x -> Maybe x
efFindRoot x ef = ILM.lookup x (efBwd ef) <|> if ILM.member x (efFwd ef) then Just x else Nothing

efFindLeaves :: Coercible x Int => x -> EquivFind x -> Maybe (IntLikeSet x)
efFindLeaves x ef = ILM.lookup x (efFwd ef)

-- | Returns an EquivFind subset representing the given list of keys
efSubset :: Coercible x Int => [x] -> EquivFind x -> EquivFind x
efSubset xs0 ef0 = foldl' go efNew xs0 where
  go (EquivFind fwd1 bwd1) x =
    let r = efLookupRoot x ef0
        ls = efLookupLeaves r ef0
    in EquivFind (ILM.insert r ls fwd1) (foldl' (\b l -> ILM.insert l r b) bwd1 (ILS.toList ls))

-- | Like 'efFindRoot' but returns same key if not found - does not guarantee presence in map
efLookupRoot :: Coercible x Int => x -> EquivFind x -> x
efLookupRoot x = fromMaybe x . ILM.lookup x . efBwd

-- | Like 'efFindLeaves' but returns empty set if not found - does not guarantee presence in map
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

efUnsafeMerge :: (Coercible x Int, Ord x) => x -> x -> EquivFind x -> (x, IntLikeSet x, EquivFind x)
efUnsafeMerge ix jx (EquivFind fwd bwd) =
  let loKey = min ix jx
      hiKey = max ix jx
      hiSet = ILS.insert hiKey (ILM.partialLookup hiKey fwd)
      finalFwd = ILM.adjust (hiSet <>) loKey (ILM.delete hiKey fwd)
      finalBwd = foldl' (flip (`ILM.insert` loKey)) bwd (ILS.toList hiSet)
  in (loKey, hiSet, EquivFind finalFwd finalBwd)

efMergeInc :: (Coercible x Int, Ord x) => x -> x -> EquivFind x -> EquivMergeRes x
efMergeInc i j ef =
  case efFindRoot i ef of
    Nothing -> EquivMergeResMissing i
    Just ix ->
      case efFindRoot j ef of
        Nothing -> EquivMergeResMissing j
        Just jx ->
          if ix == jx
            then EquivMergeResUnchanged ix
            else
              let (loKey, hiSet, ef') = efUnsafeMerge ix jx ef
              in EquivMergeResChanged loKey hiSet ef'

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
  go !roots !classRemapSet ef@(EquivFind fwd bwd) css =
    case css of
      [] ->
        let finalRoots = ILS.map (`efLookupRoot` ef) roots
        in if ILS.null classRemapSet
          then EquivMergeSetsResUnchanged finalRoots
          else EquivMergeSetsResChanged finalRoots classRemapSet ef
      ds:dss ->
        case ILS.toList ds of
          [] -> go roots classRemapSet ef dss
          zs -> case efFindAll zs ef of
            Left x -> EquivMergeSetsResMissing x
            Right xs ->
              let (loKey, ys) = fromJust (ILS.minView xs)
                  newRoots = ILS.insert loKey roots
                  hiSet = ILS.unions (fmap (\y -> ILS.insert y (efLookupLeaves y ef)) (ILS.toList ys))
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

efCanCompact :: EquivFind x -> Bool
efCanCompact = not . ILM.null . efBwd

efCompactInc :: Coercible x Int => EquivFind x -> (IntLikeMap x (IntLikeSet x), EquivFind x)
efCompactInc (EquivFind origFwd origBwd) = finalRes where
  finalRes =
    let (rootMap, fwd') = foldl' go (ILM.empty, origFwd) (ILM.elems origBwd)
    in (rootMap, EquivFind fwd' ILM.empty)
  go p@(rootMap, fwd) r =
    if ILM.member r rootMap
      then p
      else
        let xs = ILM.partialLookup r fwd
        in (ILM.insert r xs rootMap, if ILS.null xs then fwd else ILM.insert r ILS.empty fwd)

-- | Removes leaves and returns map of root -> deleted leaf
efCompact :: Coercible x Int => State (EquivFind x) (IntLikeMap x (IntLikeSet x))
efCompact = state efCompactInc
