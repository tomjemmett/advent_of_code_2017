module Day11 (day11) where

import Common
import Data.List.Split (splitOn)

day11 :: AOCSolution
day11 input = show <$> ([last, maximum] <*> pure d)
  where
    d = moveDistance <$> addMoves (move <$> splitOn "," input)

moveDistance :: Point2d -> Int
moveDistance (x, y) =
  if signum x == signum y
    then abs $ x + y
    else max (abs x) (abs y)

addMoves :: [Point2d] -> [Point2d]
addMoves = scanl1 (\(a, b) (c, d) -> (a + c, b + d))

move :: String -> Point2d
move = \case
  "n" -> (1, 0)
  "s" -> (-1, 0)
  "ne" -> (0, 1)
  "nw" -> (1, -1)
  "se" -> (-1, 1)
  "sw" -> (0, -1)
