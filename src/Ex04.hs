{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Ex04 where

import Protolude
import qualified Data.Text as Text
import Util (readText)
import Paths_adventofcode2020 ( getDataFileName )
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)



parsePassportToMap :: HasCallStack => Text -> Map Text Text
parsePassportToMap passport = Map.fromList fields2
 where
  fields0 = Text.strip passport
  fields1 = concatMap (Text.splitOn " ") (Text.splitOn "\n" fields0)
  fields2 = [(k, v) | field <- fields1, let [k, v] = Text.splitOn ":" field]

data Passport = Passport
  { byr :: Int
  , iyr :: Int
  , eyr :: Int
  , hgt :: (Int, LengthUnit)
  , hcl :: Text
  , ecl :: Text
  , pid :: Text
  , cid :: Maybe Text
  }
  deriving (Show)

data LengthUnit = Cm | In deriving (Show, Eq)

parseUnit :: Text -> Maybe LengthUnit
parseUnit "cm" = Just Cm
parseUnit "in" = Just In
parseUnit _ = Nothing

parseNumber :: Text -> Maybe Int
parseNumber = readText @Int

parseHeight :: Text -> Maybe (Int, LengthUnit)
parseHeight s = do
  let (heightT, unitT) = Text.break (`elem` ['i', 'c']) s
  height <- parseNumber heightT
  unit <- parseUnit unitT
  pure (height, unit)    

parseHairColor :: Text -> Maybe Text
parseHairColor s0 = do
  (pound, s1) <- Text.uncons s0
  if   pound == '#'
    && Text.length s1 == 6 
    && Text.all isAf09 s1 
  then Just s0
  else Nothing

isAf09 :: Char -> Bool
isAf09 c = c `elem` ['a'..'f'] ++ ['0'..'9']

parseEyeColor :: Text -> Maybe Text
parseEyeColor s = if s `elem` eyeColors then Just s else Nothing
  where 
    eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parseID :: Text -> Maybe Text
parseID s = if Text.length s == 9 && Text.all isDigit s then Just s else Nothing

parsePassport :: Map Text Text -> Maybe Passport
parsePassport passport = do
  byr_ <- parseNumber    =<< Map.lookup "byr" passport
  iyr_ <- parseNumber    =<< Map.lookup "iyr" passport
  eyr_ <- parseNumber    =<< Map.lookup "eyr" passport
  hgt_ <- parseHeight    =<< Map.lookup "hgt" passport
  hcl_ <- parseHairColor =<< Map.lookup "hcl" passport
  ecl_ <- parseEyeColor  =<< Map.lookup "ecl" passport
  pid_ <- parseID        =<< Map.lookup "pid" passport
  let cid_ = Map.lookup "cid" passport
  pure (Passport byr_ iyr_ eyr_ hgt_ hcl_ ecl_ pid_ cid_)

isValidPassport :: Passport -> Bool
isValidPassport Passport{..}
  | 1920 <= byr && byr <= 2002
  , 2010 <= iyr && iyr <= 2020
  , 2020 <= eyr && eyr <= 2030
  , isValidHeight hgt
  = True
isValidPassport _ = False

isValidHeight :: (Ord a, Num a) => (a, LengthUnit) -> Bool
isValidHeight (height, Cm) = 150 <= height && height <= 193
isValidHeight (height, In) = 59  <= height && height <= 76

main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex04.txt"
  unparsedPassports <- Text.splitOn "\n\n" <$> readFile filename
  let 
    psp = map parsePassportToMap unparsedPassports
    parsedPsp = mapMaybe parsePassport psp
    valids = filter isValidPassport parsedPsp

  print (length valids)

