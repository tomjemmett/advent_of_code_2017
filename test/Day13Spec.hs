module Day13Spec (spec) where

import SpecHelper

testInput =
  "0: 3\n\
  \1: 2\n\
  \4: 4\n\
  \6: 4"

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` ["24", "10"]

  it "Actual" $ do
    withFile
      "inputs/day13.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day13 actualInput `shouldBe` ["1904", "3833504"]
      )