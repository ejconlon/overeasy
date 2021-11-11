{-# LANGUAGE DeriveAnyClass #-}

-- | A Union-Find implementation using explicit equivalence classes.
-- We inevitably have to construct these classes so we might as well just do it as we go!
module Overeasy.EquivFind
  ( EquivFind
  , efFwd
  , efBwd
  , efSize
  , efTotalSize
  , efNew
  , efMember
  , efRoots
  , efElems
  , efAddInc
  , efAdd
  , efEnsureInc
  , efEnsure
  , efEquivs
  , efClosure
  , efFind
  , efPartialFind
  , EquivMergeRes (..)
  , efMergeInc
  , efMerge
  , EquivMergeManyRes (..)
  , efMergeManyInc
  , efMergeMany
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, modify', state)
import Data.Coerce (Coercible)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.MultiMap (IntLikeMultiMap)
import qualified Overeasy.IntLike.MultiMap as ILMM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Data.Foldable (foldl')

-- private ctor
data EquivFind x = EquivFind
  { efFwd :: !(IntLikeMultiMap x x)
  , efBwd :: !(IntLikeMap x x)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

efSize :: EquivFind x -> Int
efSize = ILMM.size . efFwd

-- | How many discrete members have ever been added? (Number of classes via 'efSize' is always LTE total.)
efTotalSize :: EquivFind x -> Int
efTotalSize = ILM.size . efBwd

-- | Creates a new UF
efNew :: EquivFind x
efNew = EquivFind ILMM.empty ILM.empty

efAddInc :: Coercible x Int => x -> EquivFind x -> EquivFind x
efAddInc x u@(EquivFind fwd bwd) = if ILM.member x bwd then u else EquivFind (ILMM.insert x x fwd) (ILM.insert x x bwd)

efAdd :: Coercible x Int => x -> State (EquivFind x) ()
efAdd = modify' . efAddInc

efEnsureInc :: Coercible x Int => x -> EquivFind x -> (x, EquivFind x)
efEnsureInc x u@(EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing -> (x, EquivFind (ILMM.insert x x fwd) (ILM.insert x x bwd))
    Just y -> (y, u)

efEnsure :: Coercible x Int => x -> State (EquivFind x) x
efEnsure = state . efEnsureInc

efEquivs :: Coercible x Int => x -> EquivFind x -> IntLikeSet x
efEquivs x (EquivFind fwd bwd) =
  case ILM.lookup x bwd of
    Nothing -> ILS.empty
    Just y -> ILM.partialLookup y fwd

efClosure :: Coercible x Int => [x] -> EquivFind x -> IntLikeSet x
efClosure xs ef = foldl' (\c x -> if ILS.member x c then c else efEquivs x ef <> c) ILS.empty xs

efFind :: Coercible x Int => x -> EquivFind x -> Maybe x
efFind x = ILM.lookup x . efBwd

efPartialFind :: Coercible x Int => x -> EquivFind x -> x
efPartialFind x = ILM.partialLookup x . efBwd

efFindAll :: Coercible x Int => [x] -> EquivFind x -> Either x (IntLikeSet x)
efFindAll xs (EquivFind _ bwd) = go ILS.empty xs where
  go !acc = \case
    [] -> Right acc
    y:ys ->
      case ILM.lookup y bwd of
        Nothing -> Left y
        Just z -> go (ILS.insert z acc) ys

efMember :: Coercible x Int => x -> EquivFind x -> Bool
efMember x  = ILM.member x . efBwd

efRoots :: Coercible x Int => EquivFind x -> [x]
efRoots = ILM.keys . efFwd

efElems :: Coercible x Int => EquivFind x -> [x]
efElems = ILM.keys . efBwd

-- | The result of trying to merge two elements of the 'EquivFind'
data EquivMergeRes x =
    EquivMergeResMissing !x
  | EquivMergeResUnchanged !x
  | EquivMergeResChanged !x !(IntLikeSet x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

efMergeInc :: (Coercible x Int, Ord x) => x -> x -> EquivFind x -> (EquivMergeRes x, EquivFind x)
efMergeInc i j u@(EquivFind fwd bwd) =
  case ILM.lookup i bwd of
    Nothing -> (EquivMergeResMissing i, u)
    Just ix ->
      case ILM.lookup j bwd of
        Nothing -> (EquivMergeResMissing j, u)
        Just jx ->
          if ix == jx
            then (EquivMergeResUnchanged ix, u)
            else
              let loKey = min ix jx
                  hiKey = max ix jx
                  hiSet = ILM.partialLookup hiKey fwd
                  finalFwd = ILM.adjust (hiSet <>) loKey (ILM.delete hiKey fwd)
                  finalBwd = foldr (`ILM.insert` loKey) bwd (ILS.toList hiSet)
              in (EquivMergeResChanged loKey hiSet, EquivFind finalFwd finalBwd)

efMerge :: (Coercible x Int, Ord x) => x -> x -> State (EquivFind x) (EquivMergeRes x)
efMerge i j = state (efMergeInc i j)

-- | The result of trying to merge multiple elements of the 'EquivFind'
data EquivMergeManyRes x =
    EquivMergeManyResEmpty
  | EquivMergeManyResEmbed !(EquivMergeRes x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

efMergeManyInc :: Coercible x Int => IntLikeSet x -> EquivFind x -> (EquivMergeManyRes x, EquivFind x)
efMergeManyInc cs u@(EquivFind fwd bwd) =
  case ILS.toList cs of
    [] -> (EquivMergeManyResEmpty, u)
    [h] -> (EquivMergeManyResEmbed (EquivMergeResUnchanged h), u)
    hs ->
      case efFindAll hs u of
        Left x -> (EquivMergeManyResEmbed (EquivMergeResMissing x), u)
        Right xs ->
          let (loKey, ys) = fromJust (ILS.minView xs)
          in if ILS.null ys && ILS.member loKey cs
            then (EquivMergeManyResEmbed (EquivMergeResUnchanged loKey), u)
            else
              let hiSet = foldr (\k s -> ILM.partialLookup k fwd <> s) ILS.empty (ILS.toList ys)
                  finalFwd = ILM.adjust (hiSet <>) loKey (foldr ILM.delete fwd (ILS.toList ys))
                  finalBwd = foldr (`ILM.insert` loKey) bwd (ILS.toList hiSet)
                  finalU = EquivFind finalFwd finalBwd
              in (EquivMergeManyResEmbed (EquivMergeResChanged loKey hiSet), finalU)

efMergeMany :: Coercible x Int => IntLikeSet x -> State (EquivFind x) (EquivMergeManyRes x)
efMergeMany = state . efMergeManyInc
