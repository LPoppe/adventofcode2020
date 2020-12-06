{-# LANGUAGE ViewPatterns #-}
module Ex06 where
import Protolude
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.Map as Map
import Paths_adventofcode2020 ( getDataFileName )

answersByGroup :: Text -> Set Char
answersByGroup = Set.fromList . filter Char.isLetter . Text.unpack

tally :: Text -> Map Char Int
tally t = Map.fromListWith (+) (zip (Text.unpack t) (repeat 1))

pop :: Ord k => k -> Map k v -> (Map k v, Maybe v)
pop k m = (Map.delete k m, Map.lookup k m)

popWithDefault :: Ord k => v -> k -> Map k v -> (Map k v, v)
popWithDefault v k m = (Map.delete k m, fromMaybe v (Map.lookup k m))

countUnanimous :: Text -> Int
countUnanimous answers =
  Map.size (Map.filter (== nPeople) tallied1)
 where
  tallied0 = tally (Text.strip answers)
  (tallied1, nPeople) = second (+1) (popWithDefault 0 '\n' tallied0)
  

sumAnswers :: [Text] -> Int
sumAnswers = sum . map (Set.size . answersByGroup)

sumUnanimous :: [Text] -> Int
sumUnanimous = sum . map countUnanimous

main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex06.txt"
  unparsedGroups <- Text.splitOn "\n\n" <$> readFile filename
  print (sumAnswers unparsedGroups)
  print (sumUnanimous unparsedGroups)
