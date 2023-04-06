{-# LANGUAGE DeriveAnyClass #-}

-- | See 'EquivFind'.
module Overeasy.EquivFind
  ( EquivFind
  , efFwd
  , efBwd
  , efRootsSize
  , efLeavesSize
  , efTotalSize
  , efCanonicalize
  , efCanonicalizePartial
  , efNew
  , efSingleton
  , efMember
  , efRoots
  , efLeaves
  , efMembers
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
  , efRemoveAllInc
  , efRemoveAll
  , efUnsafeAddLeafInc
  )
where

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

-- | A "Union-Find" implementation using explicit equivalence classes.
-- Sure, the asympotics aren't as good, but we eventually have to construct these
-- classes, so we might as well just do it as we go!
data EquivFind x = EquivFind
  { efFwd :: !(IntLikeMap x (IntLikeSet x))
  -- ^ Map of root to equivalent leaves
  -- Invariant: Map keys are only roots
  -- Invariant: Sets only contain leaf keys (and not the root itself)
  , efBwd :: !(IntLikeMap x x)
  -- ^ Map of leaf to root
  -- Invariant: Map keys are only leaves, values are only roots
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Number of roots in the equiv.
efRootsSize :: EquivFind x -> Int
efRootsSize = ILM.size . efFwd

-- | Number of leaves in the equiv.
efLeavesSize :: EquivFind x -> Int
efLeavesSize = ILM.size . efBwd

-- | Total number of keys in the equiv.
efTotalSize :: EquivFind x -> Int
efTotalSize ef = efRootsSize ef + efLeavesSize ef

-- | Canonicalize the given expression functor by replacing leaves with roots.
-- If any elements are missing, the first is returned.
efCanonicalize :: (Traversable f, Coercible x Int) => f x -> EquivFind x -> Either x (f x)
efCanonicalize fx ef = traverse (\x -> maybe (Left x) pure (efFindRoot x ef)) fx

-- | Canonicalize the given expression functor by replacing leaves with roots.
-- If any elements are missing, they are simply skipped.
efCanonicalizePartial :: (Functor f, Coercible x Int) => f x -> EquivFind x -> f x
efCanonicalizePartial fx ef = fmap (`efLookupRoot` ef) fx

-- | Creates an empty equiv
efNew :: EquivFind x
efNew = EquivFind ILM.empty ILM.empty

-- | Creates a singleton equiv
efSingleton :: Coercible x Int => x -> EquivFind x
efSingleton x = EquivFind (ILM.singleton x ILS.empty) ILM.empty

-- private
allocMM :: Coercible x Int => x -> IntLikeMap x (IntLikeSet x) -> IntLikeMap x (IntLikeSet x)
allocMM = ILM.alter (<|> Just ILS.empty)

-- private
insertMM :: Coercible x Int => x -> x -> IntLikeMap x (IntLikeSet x) -> IntLikeMap x (IntLikeSet x)
insertMM x y = ILM.alter (\case Nothing -> Just (ILS.singleton y); Just s -> Just (ILS.insert y s)) x

-- | Result of adding something to the equiv, if you're interested.
data EquivAddRes x
  = EquivAddResAlreadyRoot
  | EquivAddResAlreadyLeafOf !x
  | EquivAddResNewRoot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Add the given key to the equiv (raw version).
efAddInc :: Coercible x Int => x -> EquivFind x -> (EquivAddRes x, EquivFind x)
efAddInc x ef@(EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing ->
      if ILM.member x fwd
        then (EquivAddResAlreadyRoot, ef)
        else (EquivAddResNewRoot, EquivFind (ILM.insert x ILS.empty fwd) bwd)
    Just y -> (EquivAddResAlreadyLeafOf y, ef)

-- | Add the given key to the equiv (raw version).
efAdd :: Coercible x Int => x -> State (EquivFind x) (EquivAddRes x)
efAdd = state . efAddInc

-- | All keys equivalent to the given key in the equiv.
-- Always returns a set with the given key, even if it's not present.
efEquivs :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efEquivs x ef = let r = efLookupRoot x ef in ILS.insert r (efLookupLeaves r ef)

-- | Set of all keys equivalent to the given keys in the equiv.
efClosure :: Coercible x Int => [x] -> EquivFind x -> IntLikeSet x
efClosure xs ef = foldl' (\c x -> if ILS.member x c then c else ILS.union (efEquivs x ef) c) ILS.empty xs

-- | Find the root equivalent to the given key (if it exists).
efFindRoot :: Coercible x Int => x -> EquivFind x -> Maybe x
efFindRoot x ef = ILM.lookup x (efBwd ef) <|> if ILM.member x (efFwd ef) then Just x else Nothing

-- | Find the leaves equivalent to the given key (if they exist).
efFindLeaves :: Coercible x Int => x -> EquivFind x -> Maybe (IntLikeSet x)
efFindLeaves x ef = ILM.lookup x (efFwd ef)

-- | Returns an EquivFind subset representing the given list of keys.
efSubset :: Coercible x Int => [x] -> EquivFind x -> EquivFind x
efSubset xs0 ef0 = foldl' go efNew xs0
 where
  go (EquivFind fwd1 bwd1) x =
    let r = efLookupRoot x ef0
        ls = efLookupLeaves r ef0
    in  EquivFind (ILM.insert r ls fwd1) (foldl' (\b l -> ILM.insert l r b) bwd1 (ILS.toList ls))

-- | Like 'efFindRoot' but returns same key if not found - does not guarantee presence in map.
efLookupRoot :: Coercible x Int => x -> EquivFind x -> x
efLookupRoot x = fromMaybe x . ILM.lookup x . efBwd

-- | Like 'efFindLeaves' but returns empty set if not found - does not guarantee presence in map.
efLookupLeaves :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efLookupLeaves x = fromMaybe ILS.empty . ILM.lookup x . efFwd

-- | Returns the set of roots for the given set of keys, or an error with the first key
-- not found in the equiv.
efFindAll :: Coercible x Int => [x] -> EquivFind x -> Either x (IntLikeSet x)
efFindAll xs ef = go ILS.empty xs
 where
  go !acc = \case
    [] -> Right acc
    y : ys ->
      case efFindRoot y ef of
        Nothing -> Left y
        Just z -> go (ILS.insert z acc) ys

-- | Is the key in the equiv?
efMember :: Coercible x Int => x -> EquivFind x -> Bool
efMember x (EquivFind fwd bwd) = ILM.member x fwd || ILM.member x bwd

-- | List all roots in the equiv.
efRoots :: Coercible x Int => EquivFind x -> [x]
efRoots = ILM.keys . efFwd

-- | List all leaves in the equiv.
efLeaves :: Coercible x Int => EquivFind x -> [x]
efLeaves = ILM.keys . efBwd

-- | List all members (roots and leaves) in the equiv.
efMembers :: Coercible x Int => EquivFind x -> [x]
efMembers ef = efRoots ef ++ efLeaves ef

-- | The result of trying to merge two keys, if you care.
data EquivMergeRes x
  = EquivMergeResMissing !x
  | EquivMergeResUnchanged !x
  | EquivMergeResChanged !x !(IntLikeSet x) !(EquivFind x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Don't even think about it, it's got unsafe in the name.
efUnsafeMerge :: (Coercible x Int, Ord x) => x -> x -> EquivFind x -> (x, IntLikeSet x, EquivFind x)
efUnsafeMerge ix jx (EquivFind fwd bwd) =
  let loKey = min ix jx
      hiKey = max ix jx
      hiSet = ILS.insert hiKey (ILM.partialLookup hiKey fwd)
      finalFwd = ILM.adjust (hiSet <>) loKey (ILM.delete hiKey fwd)
      finalBwd = foldl' (flip (`ILM.insert` loKey)) bwd (ILS.toList hiSet)
  in  (loKey, hiSet, EquivFind finalFwd finalBwd)

-- | Merge two keys (raw version).
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
              in  EquivMergeResChanged loKey hiSet ef'

-- | Merge two keys (state version).
efMerge :: (Coercible x Int, Ord x) => x -> x -> State (EquivFind x) (Maybe (x, IntLikeSet x))
efMerge i j = state $ \ef ->
  case efMergeInc i j ef of
    EquivMergeResChanged loKey hiSet ef' -> (Just (loKey, hiSet), ef')
    _ -> (Nothing, ef)

-- | The result of trying to merge multiple keys, if you care.
data EquivMergeManyRes x
  = EquivMergeManyResEmpty
  | EquivMergeManyResEmbed !(EquivMergeRes x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The result of trying to merge multiple sets of keys, if you care.
data EquivMergeSetsRes x
  = EquivMergeSetsResEmptySet
  | EquivMergeSetsResMissing !x
  | EquivMergeSetsResUnchanged !(IntLikeSet x)
  | EquivMergeSetsResChanged !(IntLikeSet x) !(IntLikeSet x) !(EquivFind x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Merge sets of keys (raw version).
efMergeSetsInc :: Coercible x Int => [IntLikeSet x] -> EquivFind x -> EquivMergeSetsRes x
efMergeSetsInc css0 u0 = res
 where
  res =
    case css0 of
      [] -> EquivMergeSetsResUnchanged ILS.empty
      _ -> go ILS.empty ILS.empty u0 css0
  go !roots !classRemapSet ef@(EquivFind fwd bwd) css =
    case css of
      [] ->
        let finalRoots = ILS.map (`efLookupRoot` ef) roots
        in  if ILS.null classRemapSet
              then EquivMergeSetsResUnchanged finalRoots
              else EquivMergeSetsResChanged finalRoots classRemapSet ef
      ds : dss ->
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
              in  go newRoots newClassRemapSet newU dss

-- | Merge sets of keys (state version).
efMergeSets :: Coercible x Int => [IntLikeSet x] -> State (EquivFind x) (Maybe (IntLikeSet x, IntLikeSet x))
efMergeSets css = state $ \ef ->
  case efMergeSetsInc css ef of
    EquivMergeSetsResChanged roots classRemapSet ef' -> (Just (roots, classRemapSet), ef')
    _ -> (Nothing, ef)

-- | Are they compactible keys?
efCanCompact :: EquivFind x -> Bool
efCanCompact = not . ILM.null . efBwd

-- | See 'efCompact' (this is the raw version).
efCompactInc :: Coercible x Int => EquivFind x -> (IntLikeMap x (IntLikeSet x), EquivFind x)
efCompactInc (EquivFind origFwd origBwd) = finalRes
 where
  finalRes =
    let (rootMap, fwd') = foldl' go (ILM.empty, origFwd) (ILM.elems origBwd)
    in  (rootMap, EquivFind fwd' ILM.empty)
  go p@(rootMap, fwd) r =
    if ILM.member r rootMap
      then p
      else
        let xs = ILM.partialLookup r fwd
        in  (ILM.insert r xs rootMap, if ILS.null xs then fwd else ILM.insert r ILS.empty fwd)

-- | Removes leaves and returns map of root to deleted leaf.
efCompact :: Coercible x Int => State (EquivFind x) (IntLikeMap x (IntLikeSet x))
efCompact = state efCompactInc

-- | See 'efRemoveAll' (this is the raw version).
efRemoveAllInc :: Coercible x Int => [x] -> EquivFind x -> (IntLikeMap x x, EquivFind x)
efRemoveAllInc xs (EquivFind fwd0 bwd0) = (remapFinal, EquivFind fwdFinal bwdFinal)
 where
  (fwdFinal, bwdFinal, remapFinal) = foldl' go (fwd0, bwd0, ILM.empty) xs
  go tup@(fwd, bwd, remap) x =
    case ILM.lookup x fwd of
      -- Key is not root
      Nothing -> case ILM.lookup x bwd of
        -- Key is missing, skip it
        Nothing -> tup
        -- Key is leaf, remove from both containers
        Just r ->
          let bwd' = ILM.delete x bwd
              fwd' = ILM.adjust (ILS.delete x) r fwd
          in  (fwd', bwd', remap)
      -- Key is root
      Just leaves ->
        -- ensure the remapping is from ORIGINAL roots to new roots
        let origRoot = fromMaybe x (ILM.lookup x bwd0)
        in  case ILS.minView leaves of
              -- Singleton root, remove from fwd and remap
              Nothing ->
                let fwd' = ILM.delete x fwd
                    remap' = ILM.delete origRoot remap
                in  (fwd', bwd, remap')
              -- Non-singleton root, rotate
              Just (y, rest) ->
                let fwd' = ILM.delete x (ILM.insert y rest fwd)
                    bwd' = ILM.delete y (foldl' (\m l -> ILM.insert l y m) bwd (ILS.toList rest))
                    remap' = ILM.insert origRoot y remap
                in  (fwd', bwd', remap')

-- | Removes the given keys from the equiv map.
-- If a key is a leaf or singleton root, simply remove it.
-- If it is a root of a larger class, select the min leaf and make it root.
-- Returns a map of old roots to new roots (only those changed in the process -
-- possibly empty). If a key is not found, it is simply ignored.
efRemoveAll :: Coercible x Int => [x] -> State (EquivFind x) (IntLikeMap x x)
efRemoveAll = state . efRemoveAllInc

-- | Given root, add leaf. Requires that root be present in the map
-- and that leaf would be picked as a leaf. (Therefore, unsafe.)
-- Exposed for efficient merging.
efUnsafeAddLeafInc :: Coercible x Int => x -> x -> EquivFind x -> EquivFind x
efUnsafeAddLeafInc root leaf ef@(EquivFind fwd bwd) =
  let trueRoot = efLookupRoot root ef
  in  EquivFind (ILM.adjust (ILS.insert leaf) trueRoot fwd) (ILM.insert leaf trueRoot bwd)
