module Day16 (day16) where

import Common
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import Data.List (splitAt)
import Data.List.Split (splitOn)

day16 :: AOCSolution
day16 input = [p1, p2] <*> pure (parseInput input)
  where
    p1 = foldl (flip go) ['a' .. 'p']
    p2 = run M.empty M.empty 0 ['a' .. 'p'] . cycle . zip [0 ..]

spin :: String -> String -> String
spin x xs = b ++ a
  where
    n = length xs - read x
    (a, b) = splitAt n xs

exchange :: String -> String -> String
exchange x xs = partner (a : '/' : [b]) xs
  where
    [a, b] = map ((xs !!) . read) $ splitOn "/" x

partner :: String -> String -> String
partner (x : '/' : [y]) = map pX
  where
    pX i
      | i == x = y
      | i == y = x
      | otherwise = i

go :: String -> String -> String
go (x : xs) = case x of
  's' -> spin xs
  'x' -> exchange xs
  'p' -> partner xs

run ::
  M.HashMap (Int, String) Int ->
  M.HashMap Int String ->
  Int ->
  String ->
  [(Int, String)] ->
  String
run m r n s ((k, i) : is) = case M.lookup (k, s') m of
  Just _ -> r M.! (1000000000 `mod` n - 1)
  _ -> run m' r' (succ n) s' is
  where
    s' = go i s
    m' = M.insert (k, s') n m
    r' = M.insert n s' r

parseInput :: String -> [String]
parseInput = splitOn ","
