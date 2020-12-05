{-# LANGUAGE RecordWildCards #-}
module Ex02 where

import Data.Text (splitOn, dropEnd, count)
import Protolude
import Util

import Paths_adventofcode2020 ( getDataFileName )

data ParsedLine = ParsedLine
  { minOcc :: Int
  , maxOcc :: Int
  , letter :: Text
  , passwd :: Text }
  deriving (Show)

parseLine :: HasCallStack => Text -> ParsedLine
parseLine t = ParsedLine occMin occMax letter_ passwd_
 where
  (occMin, occMax) = 
    case splitOn "-" occs of
      [mi, ma] -> (unsafeReadText mi :: Int, unsafeReadText ma :: Int)
      split -> panic ("[2] Unexpected content in line: " <> show split)

  (occs, letter_, passwd_) =
    case splitOn " " t of
      [o, l, p] -> (o, dropEnd 1 l, p)
      split -> panic ("[1] Unexpected content in line: " <> show split)

isValidPassword :: ParsedLine -> Bool
isValidPassword ParsedLine{..} = 
  let nOccs = count letter passwd in
  minOcc <= nOccs && nOccs <= maxOcc
  
main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex02.txt"
  contents <- lines <$> readFile filename
  let 
    parsedLines = map parseLine contents
    okLines = filter isValidPassword parsedLines


  print (length okLines)
  
