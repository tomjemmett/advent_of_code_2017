module Day02 (day02) where

import Common
import Data.List (sort)

day02 :: AOCSolution
day02 input = go <$> [p1, p2] <*> pure (parseInput input)

parseInput :: String -> [[Int]]
parseInput = map (reverse . sort . map read . words) . lines

go :: ([Int] -> Int) -> [[Int]] -> String
go f = show . sum . map f

p1 :: [Int] -> Int
p1 i = head i - last i

p2 :: [Int] -> Int
p2 [] = 0
p2 (x : xs) = p2 xs + sum (map fst $ filter ((== 0) . snd) $ map (divMod x) xs)