{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ex08 where

import Protolude
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import qualified Data.HashSet as HashSet 
import Data.HashSet (HashSet)
import Paths_adventofcode2020 (getDataFileName)
import Util (unsafeReadText,  indexed )


type Accumulator = Int
type InstructionPointer = Int
type SeenInstructions = HashSet Int

data ProgramState = ProgramState
  { acc :: Accumulator
  , ip :: InstructionPointer
  , seen :: SeenInstructions }

data Instruction 
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

wrapSucc :: Int -> Int -> Int
wrapSucc bound n = succ n `mod` bound

runInstr :: Int -> ProgramState -> Instruction -> ProgramState
runInstr memBound ps@ProgramState{ip, acc} = \case
  Nop _ -> ps{ip=ipSucc}
  Acc a -> ps{ip=ipSucc, acc=acc + a}
  Jmp j -> ps{ip= (ip + j) `mod` memBound}
 where
  ipSucc = wrapSucc memBound ip

run :: IntMap Instruction -> Either Accumulator Accumulator
run instrs = go (ProgramState{acc=0, ip=0, seen=HashSet.empty})
 where
  memBound = IntMap.size instrs

  go :: ProgramState -> Either Accumulator Accumulator
  go ps0@ProgramState{ip,acc,seen=seen0}
    | ip == memBound - 1 = Right acc
    | HashSet.member ip seen0 = Left acc
    | otherwise = go ps1{seen=seen1} 
   where
    seen1 = HashSet.insert ip seen0
    ps1 = runInstr memBound ps0 instr
    instr = instrs IntMap.! ip

parseInstruction :: Text -> Instruction
parseInstruction t = 
  case instrName of
    "nop" -> Nop instrNum
    "acc" -> Acc instrNum
    "jmp" -> Jmp instrNum
    _ -> panic ("Invalid instruction: " <> instrName)
  where
   [instrName, instrArg] = Text.splitOn " " t
   instrNum = unsafeReadText (fromMaybe instrArg (Text.stripPrefix "+" instrArg))

parseInstructions :: Text -> [Instruction]
parseInstructions = map parseInstruction . Text.splitOn "\n" . Text.strip

parseInstructionsToMap :: Text -> IntMap Instruction
parseInstructionsToMap = IntMap.fromList . indexed . parseInstructions
    
permuteInstructions :: [Instruction] -> [[Instruction]]
permuteInstructions [] = []
permuteInstructions (Nop x:xs) = 
  (Jmp x : xs) : map (Nop x:) (permuteInstructions xs)
permuteInstructions (Jmp x:xs) = 
  (Nop x : xs) : map (Jmp x:) (permuteInstructions xs)
permuteInstructions (x:xs) = 
  map (x:) (permuteInstructions xs)

main :: IO ()
main = do
  ex08filename <- getDataFileName "inputs/ex08.txt"
  ex08aInstrs <- parseInstructionsToMap <$> readFile ex08filename
  print (run ex08aInstrs)

  ex08bInstrs <- parseInstructions <$> readFile ex08filename
  let 
    ex08bInstrsPerms = map (IntMap.fromList . indexed) (permuteInstructions ex08bInstrs)
    ex08bSolutions = listToMaybe (rights (map run ex08bInstrsPerms))
  
  print ex08bSolutions




  