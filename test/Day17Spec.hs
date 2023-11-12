module Day17 (day17) where

import Common
import Data.List (splitAt)

day17 :: AOCSolution
day17 input = show <$> ([part1, part2] <*> pure (read input))

part1 :: Int -> Int
part1 s = f 1 1 [0]
  where
    f 2018 p xs = xs !! succ p
    f n p xs = f (succ n) p' xs'
      where
        !p' = (p + s `mod` n + 1) `mod` n
        (a, b) = splitAt p' xs
        !xs' = a ++ n : b

part2 :: Int -> Int
part2 s = f 1 1 1
  where
    f 50000001 _ v = v
    f n p v = f (succ n) p' v'
      where
        !p' = (p + s `mod` n + 1) `mod` n
        !v' = if p' == 0 then n else v
