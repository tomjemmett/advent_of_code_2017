module Day25Spec (spec) where

import SpecHelper

testInput =
  "Begin in state A.\n\
  \Perform a diagnostic checksum after 6 steps.\n\
  \\n\
  \In state A:\n\
  \  If the current value is 0:\n\
  \    - Write the value 1.\n\
  \    - Move one slot to the right.\n\
  \    - Continue with state B.\n\
  \  If the current value is 1:\n\
  \    - Write the value 0.\n\
  \    - Move one slot to the left.\n\
  \    - Continue with state B.\n\
  \\n\
  \In state B:\n\
  \  If the current value is 0:\n\
  \    - Write the value 1.\n\
  \    - Move one slot to the left.\n\
  \    - Continue with state A.\n\
  \  If the current value is 1:\n\
  \    - Write the value 1.\n\
  \    - Move one slot to the right.\n\
  \    - Continue with state A."

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["3"]

  it "Actual" $ do
    withFile
      "inputs/day25.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day25 actualInput `shouldBe` ["3578"]
      )