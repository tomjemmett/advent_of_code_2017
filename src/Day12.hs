{-# OPTIONS_GHC -Wno-x-partial #-}

module Day12 (day12) where

import Common
import Control.Monad.RWS (MonadWriter (pass))
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Text.Parsec qualified as P

day12 :: AOCSolution
day12 input = show <$> [p1, p2]
  where
    i = parseInput input
    p1 = S.size $ go i S.empty [0]
    p2 = groups i (M.keys i) 0

groups :: M.HashMap Int (S.HashSet Int) -> [Int] -> Int -> Int
groups _ [] g = g
groups m (x : xs) g = groups m xs' (g + 1)
  where
    a = go m S.empty [x]
    xs' = S.toList $ S.difference (S.fromList xs) a

go :: M.HashMap Int (S.HashSet Int) -> S.HashSet Int -> [Int] -> S.HashSet Int
go _ s [] = s
go m s (i : is) = go m s' (S.toList n ++ is)
  where
    n = m M.! i `S.difference` s
    s' = S.insert i s

parseInput :: String -> M.HashMap Int (S.HashSet Int)
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> (Int, S.HashSet Int)
parseLine = parse' p id
  where
    p = do
      a <- number
      P.string " <-> "
      b <- number `P.sepBy` P.string ", "
      return (a, S.fromList b)