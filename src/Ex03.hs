module Ex03 where

import Protolude
import Data.List.Utils(countElem)
import qualified Data.Text as Text

import Paths_adventofcode2020 ( getDataFileName )

skipRows :: Int -> [a] -> [a]
skipRows vStepSize rows = catMaybes (zipWith go [0..] rows)
  where
    go :: Int -> a -> Maybe a
    go rowNr row = if rowNr `mod` vStepSize == 0 then Just row else Nothing

traverseMap :: [Text] ->  Int -> Int -> [Bool]
traverseMap rows hStepSize vStepSize = 
    snd (mapAccumL go 0 (skipRows vStepSize rows))
  where
    go :: Int -> Text -> (Int, Bool)
    go pos row = 
      ( (pos + hStepSize) `mod` Text.length row
      , isTree (Text.index row pos) )

traverseMapCount :: [Text] ->  Int -> Int -> Int
traverseMapCount rows hStepSize vStepSize =
  countTrees (traverseMap rows hStepSize vStepSize)

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False
       
countTrees :: [Bool] -> Int
countTrees = countElem True


main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex03.txt"
  contents <- lines <$> readFile filename

  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let nTrees = map (uncurry (traverseMapCount contents)) slopes

  print (product nTrees)