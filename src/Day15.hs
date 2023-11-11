module Day15 (day15) where

import Common
import Data.Bits ((.&.))
import Text.Parsec qualified as P

day15 :: AOCSolution
day15 input = show <$> ([part1, part2] <*> pure (parseInput input))

gen :: Int -> Int -> Int
gen v x = x * v `mod` 2147483647

genA, genB :: Int -> Int
genA = gen 16807
genB = gen 48271

part1 :: (Int, Int) -> Int
part1 (a, b) = countTrue id . take 40000000 $ zipWith (==) a' b'
  where
    a' = map (.&. 0xffff) $ iterate genA a
    b' = map (.&. 0xffff) $ iterate genB b

part2 :: (Int, Int) -> Int
part2 (a, b) = countTrue id . take 5000000 $ zipWith (==) a' b'
  where
    a' = map (.&. 0xffff) $ filter ((== 0) . (`mod` 4)) $ iterate genA a
    b' = map (.&. 0xffff) $ filter ((== 0) . (`mod` 8)) $ iterate genB b

parseInput :: String -> (Int, Int)
parseInput = parse' p id
  where
    p = do
      a <- q
      P.newline
      b <- q
      pure (a, b)
    q = do
      P.string "Generator "
      P.anyChar
      P.string " starts with "
      number