{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Set as Set

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Ord, Eq)

-- Short rope for part 1
data Rope = Rope
  { rhead :: Coord,
    rtail :: Coord,
    tailVisited :: Set.Set Coord
  }
  deriving (Show)

-- Long rope for part 2
data LongRope = LongRope
  { knots :: [Coord],
    tailVisited :: Set.Set Coord
  }
  deriving (Show)

mkRope :: Rope
mkRope =
  Rope
    { rhead = Coord {x = 0, y = 0},
      rtail = Coord {x = 0, y = 0},
      tailVisited = Set.fromList [Coord {x = 0, y = 0}]
    }

mkLongRope :: LongRope
mkLongRope =
  LongRope
    { knots = replicate 10 Coord {x = 0, y = 0},
      tailVisited = Set.fromList [Coord {x = 0, y = 0}]
    }

data Move = MUp | MDown | MLeft | MRight deriving (Show)

charToMove :: Char -> Move
charToMove 'L' = MLeft
charToMove 'R' = MRight
charToMove 'U' = MUp
charToMove 'D' = MDown
charToMove c = error $ "Invalid char: " ++ [c]

parseMoves :: String -> [Move]
parseMoves s =
  L.foldl' accumulate [] $ moveTuples s
  where
    accumulate :: [Move] -> (Char, Int) -> [Move]
    accumulate lst moveTuple = lst ++ tupleToMoves moveTuple

    moveTuples :: String -> [(Char, Int)]
    moveTuples s = toTuple <$> lines s

    toTuple :: String -> (Char, Int)
    toTuple ln = (head ln, read $ tail ln)

    tupleToMoves :: (Char, Int) -> [Move]
    tupleToMoves (c, i) = replicate i $ charToMove c

moveHead :: Rope -> Move -> Rope
moveHead r@Rope {rhead} m = r {rhead = updateCoord rhead m}

updateCoord :: Coord -> Move -> Coord
updateCoord coord@Coord {x, y} m =
  case m of
    MUp -> coord {y = y - 1}
    MDown -> coord {y = y + 1}
    MLeft -> coord {x = x - 1}
    MRight -> coord {x = x + 1}

moveHeadLong :: LongRope -> Move -> LongRope
moveHeadLong rope@LongRope {knots} m =
  rope {knots = updateCoord (head knots) m : drop 1 knots}

updateTail :: Rope -> Rope
updateTail rope@Rope {rhead, rtail, tailVisited} =
  let (moveX, moveY) = knotMove rhead rtail
      newTail = Coord {x = rtail.x + moveX, y = rtail.y + moveY}
   in rope
        { rtail =
            newTail,
          tailVisited =
            Set.insert
              newTail
              tailVisited
        }

knotMove :: Coord -> Coord -> (Int, Int)
knotMove front next = (moveX, moveY)
  where
    diffX = front.x - next.x
    diffY = front.y - next.y
    moveX
      | diffX > 1 = 1
      | diffX < -1 = -1
      | diffX /= 0 && abs diffY > 1 = if diffX > 0 then 1 else -1
      | otherwise = 0
    moveY
      | diffY > 1 = 1
      | diffY < -1 = -1
      | diffY /= 0 && abs diffX > 1 = if diffY > 0 then 1 else -1
      | otherwise = 0

updateLongTail :: LongRope -> LongRope
updateLongTail rope@LongRope {knots, tailVisited} =
  let knotsTail = updateNextKnot <$> zip knots (drop 1 knots)
   in rope
        { knots =
            head knots : knotsTail,
          tailVisited =
            Set.insert
              (last knotsTail)
              tailVisited
        }
  where
    updateNextKnot :: (Coord, Coord) -> Coord
    updateNextKnot (prev, cur) = applyMove cur (knotMove prev cur)

    applyMove :: Coord -> (Int, Int) -> Coord
    applyMove c (moveX, moveY) = c {x = c.x + moveX, y = c.y + moveY}

--    .....    .....    .....
--    .....    ..H..    ..H..
--    ..H.. -> ..... -> ..T..
--    .T...    .T...    .....
--    .....    .....    .....
--
--    .....    .....    .....
--    .....    .....    .....
--    ..H.. -> ...H. -> ..TH.
--    .T...    .T...    .....
--    .....    .....    .....

main :: IO ()
main = do
  input <- B.readFile "day9_input.txt"
  let moves = parseMoves $ B.unpack input
  let rope = L.foldl' (\r m -> updateTail (moveHead r m)) mkRope moves
  putStr "Part 1: "
  print $ length rope.tailVisited
  let longRope = L.foldl' (\r m -> updateLongTail (moveHeadLong r m)) mkLongRope moves
  putStr "Part 2: "
  print $ length longRope.tailVisited
