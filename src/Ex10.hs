{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Ex10 where

import Protolude hiding (State)
import Paths_adventofcode2020 (getDataFileName)
import Util (CacheMonad, memoize, unsafeReadText, tally)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad.Trans.State.Strict as State

parse :: Text -> [Int]
parse = map (unsafeReadText @Int) . Text.splitOn "\n" . Text.strip

combis :: Int -> [Int] -> CacheMonad Integer
combis _ [] = pure 1
combis joltage adapters =
  memoize joltage (sum <$> zipWithM combis compatible nextAdapters)
 where
  compatible = takeWhile (isCompatible joltage) adapters
  nextAdapters = map (`drop` adapters) [1..]

isCompatible :: Int -> Int -> Bool
isCompatible socket adapter = adapter - socket <= 3 && adapter > socket

main :: IO ()
main = do
  input <- getDataFileName "inputs/ex10.txt"
  adapters0 <- sort . parse <$> readFile input
  let adapters1 = adapters0 <> [maximum adapters0 + 3]

  -- Part 1
  let
    diffs = zipWith (-) adapters1 (0:adapters1)
    tallied = tally diffs
    resultMaybe = do
      nOnes <- IntMap.lookup 1 tallied
      nThrees <- IntMap.lookup 3 tallied
      pure (nOnes * nThrees)

  print resultMaybe

  -- Part 2
  print (State.evalState (combis 0 adapters1) IntMap.empty)
