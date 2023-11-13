module Day18 (day18) where

import Common
import Data.HashMap.Strict qualified as M
import Data.Vector qualified as V
import Text.Parsec qualified as P

type Registers = M.HashMap Char Int
type State = (Int, Registers, [Int], [Int])

data InstructionType = FromValue Int | FromRegister Char deriving (Show)
data Instruction =
  Snd InstructionType |
  Rcv Char |
  Set Char InstructionType |
  Add Char InstructionType |
  Mul Char InstructionType |
  Mod Char InstructionType |
  Jgz InstructionType InstructionType deriving (Show)
type Instructions = V.Vector Instruction

day18 :: AOCSolution
day18 input = show <$> [p1, p2]
  where
    i = parseInput input
    (_, _, p1:_, _) = go (0, registers, [], []) i
    p2 = run (0, registers) (0, M.insert 'p' 1 registers) [] 0 i

registers :: Registers
registers = M.fromList $ (,0) <$> "abfip"

run :: (Int, Registers) -> (Int, Registers) -> [Int] -> Int -> Instructions -> Int
run (ai, am) (bi, bm) rc c xs = if null rcB
  then c
  else run a' b' rcB (c + length rcB) xs
  where
    (ai', am', rcA, _) = go (ai, am, [], reverse rc) xs
    (bi', bm', rcB, _) = go (bi, bm, [], reverse rcA) xs
    a' = (ai', am')
    b' = (bi', bm')

go :: State -> Instructions -> State
go s@(i, m, sn, rc) xs
  | i >= V.length xs = s
  | otherwise = case runInst False s $ xs V.! i of
    Left s' -> s'
    Right s' -> go s' xs

runInst :: Bool -> State -> Instruction -> Either State State
runInst isP1 x@(i, m, sn, rc) = \case
  (Snd (FromValue v)) -> Right (i', m, v:sn, rc)
  (Snd (FromRegister r)) -> runInst isP1 x $ Snd $ FromValue $ m M.! r
  --
  (Rcv r) -> rcvFn r
  --
  (Set r (FromValue v)) -> Right (i', M.insert r v m, sn, rc)
  (Set r (FromRegister t)) -> runInst isP1 x $ Set r $ FromValue $ m M.! t
  --
  (Add r (FromValue v)) -> Right (i', M.insertWith (+) r v m, sn, rc)
  (Add r (FromRegister t)) -> runInst isP1 x $ Add r $ FromValue $ m M.! t
  --
  (Mul r (FromValue v)) -> Right (i', M.insertWith (*) r v m, sn, rc)
  (Mul r (FromRegister t)) -> runInst isP1 x $ Mul r $ FromValue $ m M.! t
  --
  (Mod r (FromValue v)) -> Right (i', M.insertWith (flip mod) r v m, sn, rc)
  (Mod r (FromRegister t)) -> runInst isP1 x $ Mod r $ FromValue $ m M.! t
  --
  (Jgz (FromValue j) (FromValue v)) -> Right (if j > 0 then i + v else i', m, sn, rc)
  (Jgz (FromValue j) (FromRegister t)) -> runInst isP1 x $ Jgz (FromValue j) $ FromValue $ m M.! t
  (Jgz (FromRegister r) t) -> runInst isP1 x $ Jgz (FromValue $ m M.! r) t
  where
    i' = succ i
    (rcx:rcxs) = rc
    rcvFn r
      | null rc = Left (i, m, sn, [])
      | isP1 && m M.! r == 0 = Right (i', m, sn, rc)
      | otherwise = Right (i', M.insert r rcx m, sn, rcxs)

parseInput :: String -> Instructions
parseInput = V.fromList . map (parse' p id) . lines
  where
    p = P.choice $ P.try <$> [
      Snd <$> (P.string "snd " *> pInstructionType),
      Rcv <$> (P.string "rcv " *> P.letter),
      pInst Set "set ",
      pInst Add "add ",
      pInst Mul "mul ",
      pInst Mod "mod ",
      Jgz <$> (P.string "jgz " *> pInstructionType) <*> (P.space *> pInstructionType)]
    pInstructionType = P.try (FromValue <$> number) P.<|> (FromRegister <$> P.letter)
    pInst f s = f <$> (P.string s *> P.letter) <*> (P.space *> pInstructionType)
