{-# OPTIONS_GHC -Wno-x-partial #-}

module Day13 (day13) where

import Common
import Text.Parsec qualified as P

day13 :: AOCSolution
day13 input = show <$> ([part1, part2] <*> pure (parseInput input))

part1 :: [(Int, Int)] -> Int
part1 input = sum [d * n | (d, n) <- input, d `mod` (2 * n - 2) == 0]

part2 :: [(Int, Int)] -> Int
part2 input = head [i| i<- [0 ..], not $ caught i]
  where
    caught i = or [(d + i) `mod` (2 * n - 2) == 0 | (d, n) <- input]

parseInput :: String -> [(Int, Int)]
parseInput = map parseLine . lines

parseLine :: String -> (Int, Int)
parseLine = parse' p id
  where
    p = do
      a <- number
      P.string ": "
      b <- number
      pure (a, b)
