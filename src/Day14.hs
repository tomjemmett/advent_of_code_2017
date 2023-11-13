{-# OPTIONS_GHC -Wno-x-partial #-}

module Day14 (day14) where

import Common
import Data.HashSet qualified as S
import KnotHash (hashString)
import Numeric (showHex)

day14 :: AOCSolution
day14 input = show <$> ([part1, part2 0] <*> pure (makeGrid input))

part1 :: S.HashSet Point2d -> Int
part1 = S.size

part2 :: Int -> S.HashSet Point2d -> Int
part2 n s
  | S.null s = n
  | otherwise = part2 (succ n) $ deleteAdjacents s [i]
  where
    i = head $ S.toList s

deleteAdjacents :: S.HashSet Point2d -> [Point2d] -> S.HashSet Point2d
deleteAdjacents s [] = s
deleteAdjacents s (i : is)
  | i `S.member` s = deleteAdjacents s' (n ++ is)
  | otherwise = deleteAdjacents s is
  where
    n = point2dNeighbours i
    s' = S.delete i s

makeGrid :: String -> S.HashSet Point2d
makeGrid i = S.fromList $ gridCoords $ map (hash i) [0 .. 127]
  where
    gridCoords g = [(r, c) | (r, vs) <- zip [0 ..] g, (c, v) <- zip [0 ..] vs, v == '#']

hash :: String -> Int -> String
hash s i =
  concatMap charToGrid
    . concatMap (`showHex` "")
    . concatMap (untuplify2 . (`divMod` 16))
    $ hashString (s ++ "-" ++ show i)

charToGrid :: Char -> String
charToGrid = \case
  '0' -> "...."
  '1' -> "...#"
  '2' -> "..#."
  '3' -> "..##"
  '4' -> ".#.."
  '5' -> ".#.#"
  '6' -> ".##."
  '7' -> ".###"
  '8' -> "#..."
  '9' -> "#..#"
  'a' -> "#.#."
  'b' -> "#.##"
  'c' -> "##.."
  'd' -> "##.#"
  'e' -> "###."
  'f' -> "####"