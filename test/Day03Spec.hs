module Day03Spec (spec) where

import SpecHelper

testInput = "1024"

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["31", "1968"]

  it "Actual" $ do
    withFile
      "inputs/day03.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day03 actualInput `shouldBe` ["438", "266330"]
      )