{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Ex10 where
import Protolude
import Paths_adventofcode2020 (getDataFileName)
import Util (unsafeReadText, tally)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap

type Cache = IntMap Integer

parse :: Text -> [Int]
parse = map (unsafeReadText @Int) . Text.splitOn "\n" . Text.strip

combis :: Cache -> (Int, [Int]) -> (Cache, Integer)
combis cache (_, []) = (cache, 1)
combis cache0 (joltage, adapters) =
  case IntMap.lookup joltage cache0 of
    Just r -> (cache0, r)
    Nothing -> (IntMap.insert joltage result cache1, result)
 where
  (cache1, sum -> result) = mapAccumL combis cache0 toTry
  compatible = takeWhile (isCompatible joltage) adapters
  nextAdapters = map (`drop` adapters) [1..]
  toTry = zip compatible nextAdapters


isCompatible :: Int -> Int -> Bool
isCompatible socket adapter = adapter - socket <= 3 && adapter > socket


main :: IO ()
main = do
  input <- getDataFileName "inputs/ex10.txt"
  adapters0 <- sort . parse <$> readFile input
  let 
    adapters1 = 0 : adapters0 <> [maximum adapters0 + 3]
    diffs = zipWith (-) (drop 1 adapters1) adapters1
    tallied = tally diffs
    resultMaybe = do
      nOnes <- IntMap.lookup 1 tallied
      nThrees <- IntMap.lookup 3 tallied
      pure (nOnes * nThrees)

  print resultMaybe
 
  print (snd (combis IntMap.empty (0, drop 1 adapters1)))
