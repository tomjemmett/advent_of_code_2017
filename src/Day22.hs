{-# OPTIONS_GHC -Wno-x-partial #-}

module Day22 (day22) where

import Common
import Control.Monad.Writer
import Data.Bifunctor (bimap, first, second)
import Data.HashMap.Strict qualified as M
import Data.Monoid

data Direction = FU | FD | FR | FL deriving (Show)
data State = Clean | Weakened | Infected | Flagged deriving (Show)

type Grid = M.HashMap Point2d State

day22 :: AOCSolution
day22 input = f <$> [p1, p2]
  where
    i = parseInput input
    start = bimap (`div` 2) (`div` 2) $ maximum $ M.keys i
    p1 = (10000, part1)
    p2 = (10000000, part2)
    f x = show . getSum . execWriter $ go x (i, start, FU)

part1, part2 :: State -> State
part1 = \case
  Clean -> Infected
  Infected -> Clean
part2 = \case
  Clean -> Weakened
  Weakened -> Infected
  Infected -> Flagged
  Flagged -> Clean

parseInput :: String -> Grid
parseInput i = M.fromList [
  ((r, c), if v == '.' then Clean else Infected)
  | (r, vs) <- zip [0..] $ lines i
  , (c, v) <- zip [0..] vs]

go :: (Int, State -> State) -> (Grid, Point2d, Direction) -> Writer (Sum Int) (Grid, Point2d, Direction)
go (0, _) s = pure s 
go (n, fn) (i, p, d) = do
  tell $ Sum (nextN v')
  go (pred n, fn) (i', p', d')
  where
    v = M.lookupDefault Clean p i
    v' = fn v
    nextN = \case
      Infected -> 1
      _ -> 0
    d' = turn v d
    p' = move d' p
    i' = M.insert p v' i

turn :: State -> Direction -> Direction
turn Clean = \case
  FU -> FL
  FL -> FD
  FD -> FR
  FR -> FU
turn Infected = \case
  FU -> FR
  FR -> FD
  FD -> FL
  FL -> FU
turn Weakened = id
turn Flagged = \case
  FU -> FD
  FR -> FL
  FD -> FU
  FL -> FR

move :: Direction -> Point2d -> Point2d
move = \case
  FU -> first pred
  FD -> first succ
  FL -> second pred
  FR -> second succ
