{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ex13 where

import Protolude hiding (Either(Left,Right))
import Paths_adventofcode2020 (getDataFileName)
import qualified Data.Text as Text
import Util (readText, unsafeReadText )
import Data.List (foldl1, zipWith3)


parse :: HasCallStack => Text -> (Int, [Maybe Int])
parse t = (unsafeReadText toa, busLines)
 where
  busLines = map readText (Text.splitOn "," unparsedBusLines)
  [toa, unparsedBusLines] = Text.splitOn "\n" (Text.strip t)

earliestBus :: Int -> [Int] -> (Int, Int)
earliestBus toa busLines = 
  minimumBy (compare `on` snd) waitTimes
 where
  waitTimes = zip busLines (map (calcWaitTime toa) busLines)
  
calcWaitTime :: Integral a => a -> a -> a
calcWaitTime toa bus = calcEarliest toa bus - toa

calcEarliest :: Integral a => a -> a -> a
calcEarliest toa bus = (toa `div` bus) * bus + bus
  
scheduleBus :: Maybe Int -> Int -> Maybe (Integer, Integer)
scheduleBus Nothing _pos = Nothing
scheduleBus (Just n) pos = Just (fromIntegral pos, fromIntegral n)


-- | Smallest 'x' such that:
--
--   n * x `mod` m == 1
--
-- AFAIK the only way to do is to simply try all combinations.
inverse ::
  -- n
  Integer ->
  -- m
  Integer ->
  -- x
  Maybe Integer
inverse n0 m = listToMaybe (mapMaybe go [1..m-1])
 where
  n1 = n0 `mod` m
  go x
    | (n1 * x) `mod` m == 1 = Just x
    | otherwise = Nothing

unsafeInverse :: HasCallStack => Integer -> Integer -> Integer
unsafeInverse n m =
  case inverse n m of
    Just x -> x
    Nothing ->
      panic ("No solution for: " <> show n <> " * x `mod` " <> show m <> " == 1")

prod3 :: Integer -> Integer -> Integer -> Integer
prod3 a b c = a * b * c

-- Solve Chinese Remainder Theorem for given system. May panic if mods aren't
-- coprime.
solveCrt :: HasCallStack => [(Integer, Integer)] -> Integer
solveCrt system = xPrime `mod` nPrime
 where
  (remainders, mods) = unzip system

  nPrime = product mods
  nPrimes = map (nPrime `div`) mods
  nPrimeInverses = zipWith unsafeInverse nPrimes mods

  xPrime = sum (zipWith3 prod3 remainders nPrimes nPrimeInverses)


main :: IO ()
main = do
  input <- getDataFileName "inputs/ex13.txt"
  (toa, buslines) <- parse <$> readFile input

  let (bus, waitTime) = earliestBus toa (catMaybes buslines)
  print (bus * waitTime)

  let busses = catMaybes (zipWith scheduleBus buslines [0..])
  print (foldl1 lcm (map snd busses) - solveCrt busses)