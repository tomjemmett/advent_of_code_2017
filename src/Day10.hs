module Day10 (day10) where

import Common
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (splitAt)
import Data.List.Split (chunksOf)
import Numeric (showHex)

day10 :: AOCSolution
day10 input = [part1, part2] <*> pure input

part1 :: String -> String
part1 = show . f . go (0, 0, [0 .. 255]) . commaSeparatedInts
  where
    f (_, _, x : y : _) = x * y

part2 :: String -> String
part2 input = toHex . dense . sparse $ knot 64 (0, 0, [0 .. 255])
  where
    i = (ord <$> input) ++ [17, 31, 73, 47, 23]
    knot 0 (_, _, x) = x
    knot n x = knot (n - 1) $ go x i
    sparse = chunksOf 16
    dense = map (foldl1 xor)
    toHex = concatMap (`showHex` "")

go :: (Int, Int, [Int]) -> [Int] -> (Int, Int, [Int])
go = foldl f
  where
    f :: (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
    f (skip, pos, xs) i = (skip + 1, (skip + pos + i) `mod` 256, r)
      where
        (reverse -> a, take (256 - i) -> b) = splitAt i $ drop pos $ cycle xs
        r = take 256 $ drop (256 - pos) $ cycle (a ++ b)
