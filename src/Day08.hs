module Day08 (day08) where

import Common
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximumBy)
import Text.Parsec qualified as P

type Registers = M.HashMap String Int

type Instruction = Registers -> Registers

day08 :: AOCSolution
day08 input = show <$> ([last, maximum] <*> pure r)
  where
    i = parseInput input
    r = go i

go :: [Instruction] -> [Int]
go =
  map (maximum . map snd . M.toList)
    . tail
    . scanl (\m i -> i m) M.empty

insertWith0 ::
  (Int -> Int -> Int) ->
  String ->
  Int ->
  M.HashMap String Int ->
  M.HashMap String Int
insertWith0 f k v m = M.insert k new m
  where
    current = M.lookupDefault 0 k m
    new = current `f` v

parseInput :: String -> [Instruction]
parseInput = map (p . words) . lines
  where
    p :: [String] -> Registers -> Registers
    p
      [ instR,
        pInst -> instT,
        read -> instV,
        "if",
        compR,
        pComp -> compT,
        read -> compV
        ]
      m =
        if M.lookupDefault 0 compR m `compT` compV
          then insertWith0 instT instR instV m
          else m
    pInst = \case
      "inc" -> (+)
      "dec" -> (-)
    pComp = \case
      ">" -> (>)
      ">=" -> (>=)
      "<" -> (<)
      "<=" -> (<=)
      "==" -> (==)
      "!=" -> (/=)
