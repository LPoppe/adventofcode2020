{-# LANGUAGE RankNTypes #-}
module Util where

import Protolude

import qualified Data.IntMap as IntMap 

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
