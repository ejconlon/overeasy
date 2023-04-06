-- | Stuff for streaming search results.
module Overeasy.Streams
  ( chooseWith
  , choose
  , Stream
  , streamAll
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Logic (LogicT, MonadLogic, observeAllT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), State, runState)

newtype M r s a = M {unM :: ReaderT r (State s) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState s
    )

runM :: M r s a -> r -> s -> (a, s)
runM m r = runState (runReaderT (unM m) r)

-- | Choose one of many alteratives and process it with the given function.
chooseWith :: (Foldable f, Alternative m) => f a -> (a -> m b) -> m b
chooseWith fa f = foldr ((<|>) . f) empty fa

-- | Choose one of many alteratives.
choose :: (Foldable f, Alternative m) => f a -> m a
choose fa = chooseWith fa pure

-- | A stream of results. Just a wrapper around 'LogicT' to keep things tidy.
newtype Stream r s a = Stream {unStream :: LogicT (M r s) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState s
    , MonadLogic
    , Alternative
    , MonadPlus
    , Semigroup
    , Monoid
    )

-- | Produces all results from the stream.
streamAll :: Stream r s a -> r -> s -> [a]
streamAll stream env st = fst (runM (observeAllT (unStream stream)) env st)
