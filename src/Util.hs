{-# LANGUAGE RankNTypes #-}
module Util where

import Protolude hiding (State)

import qualified Data.IntMap as IntMap
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (State)

readText :: forall a. Read a => Text -> Maybe a
readText = readMaybe . toS

unsafeReadText :: (HasCallStack, Read a) => Text -> a
unsafeReadText = fromMaybe (panic "wut") . readText

andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen = (=<<)

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

-- | Count the number of times an element occurs in a list
tally :: [Int] -> IntMap Int
tally xs = IntMap.fromListWith (+) (zip xs (repeat 1))

type CacheMonad a = State (IntMap a) a

memoize :: Int -> CacheMonad a -> CacheMonad a
memoize key action = do
  cached <- IntMap.lookup key <$> State.get
  case cached of
    Just c -> pure c
    Nothing -> do
      res <- action
      State.modify (IntMap.insert key res)
      pure res
