{-# LANGUAGE LambdaCase #-}
module Ex05 where
import Protolude
import qualified Data.Text as Text
import Paths_adventofcode2020 ( getDataFileName )

import qualified Data.Set as Set

-- | Ranges are up to but not including
type Range = (Int, Int)
data Side = Low | High

halveRange :: Range -> Side -> Range
halveRange (l, h) Low = (l, l + ((h-l) `div` 2))
halveRange (l, h) High = (l + ((h-l) `div` 2), h)

halveRanges :: Range -> [Side] -> Range
halveRanges = foldl halveRange

parsePartioning :: HasCallStack => Text -> [Side]
parsePartioning t = map parseSide (Text.unpack t)

parseSide :: HasCallStack => Char -> Side
parseSide = \case
  'F' -> Low
  'L' -> Low
  'B' -> High
  'R' -> High
  c -> panic ("Got unexpected side: " <> show c)

determineSeat :: HasCallStack => Text -> (Int, Int)
determineSeat partioning = (row, column)
  where 
    (rowT, columnT) = Text.splitAt 7 partioning
    (row, _) = halveRanges (0, 128) (parsePartioning rowT)
    (column, _) = halveRanges (0, 8) (parsePartioning columnT)

determineSeatId :: HasCallStack => Text -> Int
determineSeatId partitioning = 
  let (row, column) = determineSeat partitioning in
  row * 8 + column

maxSeatId :: [Text] -> Int
maxSeatId partitionings = maximum (map determineSeatId partitionings)

findMySeat :: Set Int -> Maybe Int
findMySeat = go . Set.toList
 where
  go :: [Int] -> Maybe Int
  go [] = Nothing
  go [_] = Nothing
  go (a:b:rest) 
    | b - a == 2 = Just (b - 1)
    | otherwise = go (b:rest)

main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex05.txt"
  partitionings <- lines <$> readFile filename
  print (findMySeat (Set.fromList (map determineSeatId partitionings)))