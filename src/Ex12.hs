{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ex12 where

import Protolude hiding (Either(Left,Right))
import Paths_adventofcode2020 (getDataFileName)
import qualified Data.Text as Text
import Util ( unsafeReadText )

data Degree
  = D0
  | D90
  | D180
  | D270
  deriving (Enum, Bounded, Show)

data AbsDirection
  = North
  | East
  | South 
  | West
  deriving (Enum, Bounded, Show)

data RelDirection
  = Left
  | Right
  deriving (Enum, Bounded, Show)

data Instruction
  = GoAbs AbsDirection Int
  | GoRel Int
  | Rotate Degree
  deriving (Show)

data BoatState = BoatState
  { position :: !(Int, Int) 
  , facing :: !AbsDirection }
  deriving (Show)

addWrap :: forall a. (Enum a, Bounded a) => Int -> a -> a
addWrap n a = toEnum ((n + fromEnum a) `mod` (fromEnum @a maxBound + 1))

parseDegree :: HasCallStack => RelDirection -> Int -> Degree
parseDegree Left val = 
  case val of
    90 -> D270
    180 -> D180 
    270 -> D90
    _ -> panic ("Unexpected degree: " <> show val)
parseDegree Right val = 
  case val of
    90 -> D90
    180 -> D180 
    270 -> D270
    _ -> panic ("Unexpected degree: " <> show val)

parseInstruction :: HasCallStack => Text -> Instruction
parseInstruction action =
  case second (unsafeReadText @Int) <$> Text.uncons action of
    Just ('F', value) -> GoRel value
    Just ('L', value) -> Rotate (parseDegree Left value)
    Just ('R', value) -> Rotate (parseDegree Right value)
    Just ('N', value) -> GoAbs North value
    Just ('E', value) -> GoAbs East value
    Just ('W', value) -> GoAbs West value
    Just ('S', value) -> GoAbs South value
    _ -> panic ("Parse error, unexpected action: " <> action)

parseInstructions :: HasCallStack => Text -> [Instruction]
parseInstructions = map parseInstruction . Text.splitOn "\n" . Text.strip

runInstr :: BoatState -> Instruction -> BoatState
runInstr boatState@BoatState{position,facing} = \case
  GoAbs dir steps -> boatState{position=move steps position dir}
  GoRel steps -> boatState{position=move steps position facing}
  Rotate degree -> boatState{facing=addWrap (fromEnum degree) facing}

move :: Int -> (Int, Int) -> AbsDirection -> (Int, Int)
move steps (!x, !y) = \case
  North -> (x, y+steps)
  East -> (x+steps, y)
  South -> (x, y-steps)
  West -> (x-steps, y)

main :: IO ()
main = do
  input <- getDataFileName "inputs/ex12.txt"
  instructions <- parseInstructions <$> readFile input
  let 
    origin = BoatState (0, 0) East
    destination = foldl' runInstr origin instructions

  print origin
  print destination
  print (manhattanDistance (position origin) (position destination))

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)
