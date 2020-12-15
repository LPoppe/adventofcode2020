{-# LANGUAGE LambdaCase #-}
module Ex11 where

import Protolude
import Paths_adventofcode2020 (getDataFileName)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
-- import Text.Show.Pretty (ppShow)
import Data.List.Utils (countElem)
import Data.List ((\\), span)

type BoardSize = (Int, Int)
type Board = HashMap (Int, Int) Bool
type NextCellFunction = Board -> (Int, Int) -> Bool -> Bool

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Enum, Bounded)

parseCell :: Char -> Maybe Bool
parseCell '#' = Just True
parseCell 'L' = Just False
parseCell _ = Nothing

parseRow :: Int -> Text -> [((Int, Int), Bool)]
parseRow rowNr row = catMaybes (zipWith go positions cells)
 where
  positions = [(rowNr, colNr) | colNr <- [0..]]
  cells = map parseCell (Text.unpack row)

  go pos (Just cell) = Just (pos, cell)
  go _ _ = Nothing

parse :: HasCallStack => Text -> (Board, BoardSize)
parse t = (HashMap.fromList parsedRows, (nRows, nCols))
 where
  parsedRows = concat (zipWith parseRow [0..] unparsedRows)
  unparsedRows@(row:_) = Text.splitOn "\n" (Text.strip t)
  nCols = Text.length row - 1
  nRows = length unparsedRows - 1

relDirections :: Direction -> [(Int, Int)]
relDirections = \case
  North ->     zip [-1,-2..]  (repeat 0)
  NorthEast -> zip [-1,-2..]  [1,2..]
  East ->      zip (repeat 0) [1,2..]
  SouthEast -> zip [1,2..]    [1,2..]
  South ->     zip [1,2..]    (repeat 0)
  SouthWest -> zip [1,2..]    [-1,-2..]
  West ->      zip (repeat 0) [-1,-2..]
  NorthWest -> zip [-1,-2..]  [-1,-2..]

absDirections :: BoardSize -> (Int, Int) -> Direction -> [(Int, Int)]
absDirections (maxRows, maxCols) (r, c) dir = validAbs
  where
    (validAbs, _) = span isValidPos allAbs
    isValidPos (x, y) = x >= 0 && y >= 0 && x <= maxRows && y <= maxCols
    allAbs =  [(r+rRel, c+cRel) | (rRel, cRel) <- relDirections dir]

relNeighbors :: [(Int, Int)]
relNeighbors = [(row, col) | row <- [-1..1], col <- [-1..1]] \\ [(0, 0)]

absNeighbors :: (Int, Int) -> [(Int, Int)]
absNeighbors (r, c) = [(r+rRel, c+cRel) | (rRel, cRel) <- relNeighbors]

nextCellB :: BoardSize -> NextCellFunction
nextCellB boardSize board pos meOccupied 
  | not meOccupied && occupied == 0 = True
  | occupied >= 5 = False
  | otherwise = meOccupied
 where
  neighbors = 
    [ neighbor 
    | dir <- [minBound..maxBound]
    , let line = absDirections boardSize pos dir
    , let neighbor = listToMaybe (mapMaybe (`HashMap.lookup` board) line) ]
    
  occupied = countElem True (catMaybes neighbors)

nextCellA :: NextCellFunction
nextCellA board pos meOccupied
  | not meOccupied && occupied == 0 = True
  | occupied >= 4 = False
  | otherwise = meOccupied
 where
  neighbors = mapMaybe (`HashMap.lookup` board) (absNeighbors pos)
  occupied = countElem True neighbors

nextBoard :: NextCellFunction -> Board -> Board
nextBoard nc b = HashMap.mapWithKey (nc b) b

findFixpoint :: Eq a => (a -> a) -> a -> a
findFixpoint f a = 
  let fa = f a in 
  if fa == a then a else findFixpoint f fa

main :: IO ()
main = do
  input <- getDataFileName "inputs/ex11.txt"
  (board0, boardSize) <- parse <$> readFile input
  
  let 
    board1 = findFixpoint (nextBoard nextCellA) board0
    board1occupied = countElem True (HashMap.elems board1)

    board2 = findFixpoint (nextBoard (nextCellB boardSize)) board0
    board2occupied = countElem True (HashMap.elems board2)

  print board1occupied
  print board2occupied

