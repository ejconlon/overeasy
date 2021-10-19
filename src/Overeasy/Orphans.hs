{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | These instances come from newer hashable: https://hackage.haskell.org/package/hashable-1.3.4.1/docs/Data-Hashable.html
module Overeasy.Orphans
  (
  ) where

import qualified Data.Foldable as F
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
import qualified Data.Sequence as Seq

instance Hashable v => Hashable (Seq.Seq v) where
    hashWithSalt = hashWithSalt1

instance Hashable1 Seq.Seq where
    liftHashWithSalt h s x = F.foldl' h (hashWithSalt s (Seq.length x)) x
