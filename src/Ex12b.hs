{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ex12b where

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
  = Move Int
  | GoAbs AbsDirection Int
  | Rotate Degree
  deriving (Show)

data BoatState = BoatState
  { position :: !(Int, Int) 
  , waypoint :: !(Int, Int) }
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
    Just ('F', value) -> Move value
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
runInstr boatState@BoatState{position,waypoint} = \case
  Move steps -> boatState{position=moveShip steps waypoint position}
  GoAbs dir steps -> boatState{waypoint=moveWaypoint steps waypoint dir}
  Rotate degree -> boatState{waypoint=rotateWaypoint waypoint degree}

rotateWaypoint :: (Int, Int) -> Degree -> (Int, Int)
rotateWaypoint (x, y) degree =
  ( x * cos_ degree - y * sin_ degree
  , x * sin_ degree + y * cos_ degree )
 where
  cos_ :: Degree -> Int
  cos_ D0 = 1
  cos_ D90 = 0
  cos_ D180 = -1
  cos_ D270 = 0

  sin_ :: Degree -> Int
  sin_ D0 = 0
  sin_ D90 = -1
  sin_ D180 = 0
  sin_ D270 = 1

moveShip :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveShip steps (wx, wy) (bx, by) = (bx + wx * steps, by + wy * steps)

moveWaypoint :: Int -> (Int, Int) -> AbsDirection -> (Int, Int)
moveWaypoint steps (!x, !y) = \case
  North -> (x, y+steps)
  East -> (x+steps, y)
  South -> (x, y-steps)
  West -> (x-steps, y)

main :: IO ()
main = do
  input <- getDataFileName "inputs/ex12.txt"
  instructions <- parseInstructions <$> readFile input
  let 
    origin = BoatState (0, 0) (10, 1)
    destination = foldl' runInstr origin instructions

  print origin
  print destination
  print (manhattanDistance (position origin) (position destination))

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)
