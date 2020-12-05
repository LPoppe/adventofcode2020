{-# LANGUAGE RecordWildCards #-}
module Ex02b where

import Data.Text (splitOn, dropEnd, head, index)
import Protolude hiding (head)
import Util

import Paths_adventofcode2020 ( getDataFileName )

data ParsedLine = ParsedLine
  { pos1 :: Int
  , pos2 :: Int
  , letter :: Char
  , passwd :: Text }
  deriving (Show)

parseLine :: HasCallStack => Text -> ParsedLine
parseLine t = ParsedLine pos1_ pos2_ (head letter_) passwd_
 where
  (pos1_, pos2_) = 
    case splitOn "-" poss of
      [p1, p2] -> (unsafeReadText p1 :: Int, unsafeReadText p2 :: Int)
      split -> panic ("[2] Unexpected content in line: " <> show split)

  (poss, letter_, passwd_) =
    case splitOn " " t of
      [o, l, p] -> (o, dropEnd 1 l, p)
      split -> panic ("[1] Unexpected content in line: " <> show split)

isValidPassword :: HasCallStack => ParsedLine -> Bool
isValidPassword ParsedLine{..} = 
  let
    letter1 = index passwd (pos1 - 1)
    letter2 = index passwd (pos2 - 1)
  in
    xor (letter1 == letter) (letter2 == letter)

  
main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex02.txt"
  contents <- lines <$> readFile filename
  let 
    parsedLines = map parseLine contents
    okLines = filter isValidPassword parsedLines


  print (length okLines)
  
