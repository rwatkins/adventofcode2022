{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Data.List as L
import qualified Data.List.Split as S
import Debug.Trace (traceShow)

data Instruction = Noop | Addx Int deriving (Show)

type Cycle = (Maybe Instruction, Int)

data State = State
  { x :: Int,
    cycles :: [Cycle]
  }
  deriving (Show)

emptyState :: State
emptyState = State {x = 1, cycles = []}

instructionCycles :: Instruction -> Int -> [(Maybe Instruction, Int)]
instructionCycles Noop x = [(Just Noop, x)]
instructionCycles (Addx i) x = [(Just (Addx i), x), (Nothing, x + i)]

exec :: Instruction -> State -> State
exec ins state =
  let cycles = instructionCycles ins state.x
      (_, newX) = last cycles
   in state {x = newX, cycles = state.cycles ++ cycles}

instructions :: String -> [Instruction]
instructions s = lineToInstr <$> lines s
  where
    lineToInstr :: String -> Instruction
    lineToInstr line = case words line of
      ["noop"] -> Noop
      ["addx", num] -> Addx (read num)

testInput :: IO String
testInput = readFile "test_input.txt"

testInstructions :: IO [Instruction]
testInstructions = instructions <$> testInput

execAll :: [Instruction] -> State
execAll = L.foldl' (flip exec) emptyState

indexes :: [Int]
indexes = [20, 60 .. 220]

part1 :: String -> Int
part1 input =
  let state = execAll (instructions input)
      xs = map snd state.cycles
      -- "During" the 20th cycle actually means we want the value at the *end*
      -- of the 19th cycle, and since the list is 0-index, the value during the
      -- 20th cycle is at index 18
      signals = map (\idx -> (idx - 2, xs !! (idx - 2))) indexes
      strengths = map (\(idx, x) -> (idx + 2) * x) signals
   in sum strengths

part2 :: String -> String
part2 input =
  let state = execAll (instructions input)
      xs = map snd state.cycles
      -- Something about this results in the first column of the output missing?
      -- You can still read the letter though
      xsWithCycle = zip (cycle [1 .. 40]) xs
      pixels = [if c >= x - 1 && c <= x + 1 then '#' else '.' | (c, x) <- xsWithCycle]
      lines = S.chunksOf 40 pixels
   in L.intercalate "\n" lines

main :: IO ()
main = do
  input <- readFile "day10_input.txt"
  putStr "Part 1: "
  print $ part1 input
  putStrLn "Part 2:"
  putStrLn $ part2 input
