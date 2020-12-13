{-# LANGUAGE TypeApplications #-}
module Ex09 where
import Protolude
import qualified CountSeq 
import CountSeq (CountSeq)
import Paths_adventofcode2020 (getDataFileName)
import Util (unsafeReadText)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

findWeaknessSeq :: Int -> [Int] -> Maybe [Int]
findWeaknessSeq error = go 0
 where
  go count everything
    | count == error = Just []
    | count > error = Nothing
    | (x:xs) <- everything = (x:) <$> go (count+x) xs
    | otherwise = Nothing

findWeaknesses :: Int -> [Int] -> [Int]
findWeaknesses error = 
  mapMaybe getWeakness . catMaybes . go
 where
  go [] = []
  go (x:xs) = findWeaknessSeq error (x:xs) : go xs

getWeakness :: [Int] -> Maybe Int
getWeakness [] = Nothing
getWeakness [_] = Nothing
getWeakness xs = Just (minimum xs + maximum xs)

findFails :: [Int] -> CountSeq -> [Int]
findFails xs countSeq = catMaybes (snd (mapAccumL findFail countSeq xs))

findFail :: CountSeq -> Int -> (CountSeq, Maybe Int)
findFail countSeq x = 
  ( CountSeq.shiftInR countSeq x
  , case findValid x countSeq of 
      Nothing -> Just x
      Just _ -> Nothing )

findValid :: Int -> CountSeq -> Maybe Int
findValid total countSeq = go (CountSeq.toList countSeq)
 where
  go :: [Int] -> Maybe Int
  go [] = Nothing
  go (x:xs) 
    | hasPartner total x countSeq = Just x
    | otherwise = go xs

hasPartner :: Int -> Int -> CountSeq -> Bool
hasPartner total n countSeq = 
  let minExpected = if n == total - n then 2 else 1 in
  CountSeq.lookupCount (total - n) countSeq >= minExpected

parse :: HasCallStack => Int -> Text -> (CountSeq, [Int])
parse size text = (countSeq, xmasData)
 where
  numbers = map (unsafeReadText @Int) (Text.splitOn "\n" (Text.strip text))
  (seqNr, xmasData) = List.splitAt size numbers
  countSeq = CountSeq.fromList (NonEmpty.fromList seqNr)

main :: IO ()
main = do
  input <- getDataFileName "inputs/ex09.txt"
  (countSeq, xmasData) <- parse 25 <$> readFile input
  let 
    errorMaybe = listToMaybe (findFails xmasData countSeq)
    Just error = errorMaybe
    weaknessMaybe = listToMaybe (findWeaknesses error xmasData)
    
  print errorMaybe
  print weaknessMaybe
