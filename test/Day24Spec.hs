module Day24Spec (spec) where

import SpecHelper

testInput =
  "0/2\n\
  \2/2\n\
  \2/3\n\
  \3/4\n\
  \3/5\n\
  \0/1\n\
  \10/1\n\
  \9/10"

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 testInput `shouldBe` ["31", "19"]

  it "Actual" $ do
    withFile
      "inputs/day24.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day24 actualInput `shouldBe` ["1511", "1471"]
      )