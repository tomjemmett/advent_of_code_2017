module Day12Spec (spec) where

import SpecHelper

testInput =
  "0 <-> 2\n\
  \1 <-> 1\n\
  \2 <-> 0, 3, 4\n\
  \3 <-> 2, 4\n\
  \4 <-> 2, 3, 6\n\
  \5 <-> 6\n\
  \6 <-> 4, 5"

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInput `shouldBe` ["6", "2"]

  it "Actual" $ do
    withFile
      "inputs/day12.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day12 actualInput `shouldBe` ["141", "171"]
      )