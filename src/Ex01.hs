module Ex01 where

import Protolude

import Paths_adventofcode2020 ( getDataFileName )

import qualified Data.Set as Set
import Data.List.Extra (nubOrd)

import Util ( unsafeReadText )

main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex01.txt"
  contents <- readFile filename
  let 
    split = lines contents
    input = Set.fromList (map unsafeReadText split :: [Int])
    
  print 
    $ nubOrd
    $ map orderTup3
    $ sumTo3 2020 input
    

sumTo2 :: Int -> Set Int -> [(Int, Int)]
sumTo2 target numbers = mapMaybe go (Set.toList numbers)
  where
    go n = 
      if Set.member (target - n) numbers then
        Just (n, target - n)
      else
        Nothing

sumTo3 :: Int -> Set Int -> [(Int, Int, Int)]
sumTo3 target numbers = concatMap go (Set.toList numbers)
  where
    go n = [(n, a, b) | (a, b) <- sumTo2 (target - n) numbers]


orderTup :: Ord a => (a, a) -> (a, a)
orderTup (a, b) = if a <= b then (a, b) else (b, a)

orderTup3 :: Ord a => (a, a, a) -> (a, a, a)
orderTup3 (a, b, c) =
  case sort [a, b, c] of
    [x, y, z] -> (x, y, z)
    _ -> panic "Cannot happen"