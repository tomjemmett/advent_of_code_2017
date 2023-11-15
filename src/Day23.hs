{-# OPTIONS_GHC -Wno-x-partial #-}

module Day23 (day23) where

import Common
import Data.List.Split (chunksOf)
import Data.Numbers.Primes (isPrime)

day23 :: AOCSolution
day23 input = show <$> ([part1, part2] <*> pure x)
  where
    x = read $ last $ words $ head $ lines input

part1 :: Int -> Int
part1 x = (x - 2) * (x - 2)

part2 :: Int -> Int
part2 x = length $ filter (not . isPrime . head) $ chunksOf 17 [x' .. x' + 17000]
  where
    x' = x * 100 + 100000
