{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sort (sort)
import Debug.Trace (traceShow)

-- Monkey 0:
--   Starting items: 79, 98
--   Operation: new = old * 19
--   Test: divisible by 23
--     If true: throw to monkey 2
--     If false: throw to monkey 3

type Item = Int

type PendingThrows = Map.Map Int [Int]

type InspectCounts = Map.Map Int Int

data Operation
  = Add String String
  | Mult String String
  deriving (Show)

data Monkey = Monkey
  { items :: [Item],
    operation :: Operation,
    prime :: Int,
    throwTo :: (Int, Int) -- if test i then fst throwTo else snd throwTo
  }
  deriving (Show)

data Throw = Throw {item :: Item, monkey :: Int} deriving (Show)

type World = ([Monkey], PendingThrows, InspectCounts)

divisibleBy :: Int -> Item -> Bool
divisibleBy n item = item `mod` n == 0

-- Monkey 0:
--   Monkey inspects an item with a worry level of 79.
--     Worry level is multiplied by 19 to 1501.
--     Monkey gets bored with item. Worry level is divided by 3 to 500.
--     Current worry level is not divisible by 23.
--     Item with worry level 500 is thrown to monkey 3.
--   Monkey inspects an item with a worry level of 98.
--     Worry level is multiplied by 19 to 1862.
--     Monkey gets bored with item. Worry level is divided by 3 to 620.
--     Current worry level is not divisible by 23.
--     Item with worry level 620 is thrown to monkey 3.

monkeyInspectItem :: Monkey -> Item -> (Item -> Item) -> Throw
monkeyInspectItem monkey@Monkey {operation, prime, throwTo} item reduceWorry =
  let it = applyOperation operation item
      itemDiv3 = reduceWorry it
      throwToMonkey =
        if divisibleBy prime itemDiv3
          then fst throwTo
          else snd throwTo
   in Throw {item = itemDiv3, monkey = throwToMonkey}

monkeyInspectAll :: Monkey -> (Item -> Item) -> (Monkey, [Throw])
monkeyInspectAll monkey reduceWorry =
  let throws = L.foldl' f [] monkey.items
   in (monkey {items = []}, throws)
  where
    f throws item = monkeyInspectItem monkey item reduceWorry : throws

mapSwap :: Ord k => k -> v -> Map.Map k v -> (Maybe v, Map.Map k v)
mapSwap key value m = (Map.lookup key m, Map.insert key value m)

emptyPendingThrows :: PendingThrows
emptyPendingThrows = Map.fromList [(i, []) | i <- [0 .. 7]]

emptyInspectCounts :: InspectCounts
emptyInspectCounts = Map.fromList [(i, 0) | i <- [0 .. 7]]

doRound :: World -> (Item -> Item) -> World
doRound (monkeys, pendingThrows, inspectCounts) reduceWorry =
  -- Run a single round, returning the updated list of monkeys, a map of
  -- monkeyIndex -> pending throws, and a map of monkeyIndex -> items
  -- inspected.
  L.foldl' f ([], pendingThrows, inspectCounts) $ zip [0 ..] monkeys
  where
    f :: World -> (Int, Monkey) -> World
    f (seenMonkeys, pendingThrows, counts) (monkeyIdx, monkey) =
      let (mpending, pending) = mapSwap monkeyIdx [] pendingThrows
          -- Inspect all items
          (newMonkey, throws) = monkeyInspectAll monkey {items = monkey.items ++ fromMaybe [] mpending} reduceWorry
          -- Record throws
          newPending = L.foldl' (\m th -> Map.adjust (++ [th.item]) th.monkey m) pending throws
          -- Record count of items inspected
          newInspectCounts = Map.adjust (+ length throws) monkeyIdx counts
       in (seenMonkeys ++ [newMonkey], newPending, newInspectCounts)

applyOperation :: Operation -> Item -> Item
applyOperation op item =
  case op of
    Add x y -> parse x + parse y
    Mult x y -> parse x * parse y
  where
    parse :: String -> Int
    parse "old" = item
    parse s = read s

testInput :: [Monkey]
testInput =
  [ Monkey {items = [79, 98], operation = Mult "old" "19", prime = 23, throwTo = (2, 3)},
    Monkey {items = [54, 65, 75, 74], operation = Add "old" "6", prime = 19, throwTo = (2, 0)},
    Monkey {items = [79, 60, 97], operation = Mult "old" "old", prime = 13, throwTo = (1, 3)},
    Monkey {items = [74], operation = Add "old" "3", prime = 17, throwTo = (0, 1)}
  ]

realInput :: [Monkey]
realInput =
  [ Monkey {items = [52, 78, 79, 63, 51, 94], operation = Mult "old" "13", prime = 5, throwTo = (1, 6)},
    Monkey {items = [77, 94, 70, 83, 53], operation = Add "old" "3", prime = 7, throwTo = (5, 3)},
    Monkey {items = [98, 50, 76], operation = Mult "old" "old", prime = 13, throwTo = (0, 6)},
    Monkey {items = [92, 91, 61, 75, 99, 63, 84, 69], operation = Add "old" "5", prime = 11, throwTo = (5, 7)},
    Monkey {items = [51, 53, 83, 52], operation = Add "old" "7", prime = 3, throwTo = (2, 0)},
    Monkey {items = [76, 76], operation = Add "old" "4", prime = 2, throwTo = (4, 7)},
    Monkey {items = [75, 59, 93, 69, 76, 96, 65], operation = Mult "old" "19", prime = 17, throwTo = (1, 3)},
    Monkey {items = [89], operation = Add "old" "2", prime = 19, throwTo = (2, 4)}
  ]

doRounds :: Int -> (Item -> Item) -> World -> World
doRounds n reduceWorry world
  | n > 0 = let w = doRound world reduceWorry in doRounds (n - 1) reduceWorry w
  | otherwise = world

monkeyBusiness :: Int -> (Item -> Item) -> Int
monkeyBusiness rounds reduceWorry =
  let (_, _, inspectCounts) = doRounds rounds reduceWorry (realInput, emptyPendingThrows, emptyInspectCounts)
      top2 = take 2 . reverse . sort . map snd $ Map.toList inspectCounts
   in L.foldl' (*) 1 top2

part1 :: Int
part1 = monkeyBusiness 20 (`div` 3)

part2 :: Int
part2 = monkeyBusiness 10000 (`mod` lcm')
  where
    lcm' = foldl lcm 1 $ prime <$> realInput

main :: IO ()
main = do
  --   input <- readFile "day11_input.txt"
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
