module Day15Spec (spec) where

import SpecHelper

testInput =
  "Generator A starts with 65\n\
  \Generator B starts with 8921"

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["588", "309"]

  it "Actual" $ do
    withFile
      "inputs/day15.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day15 actualInput `shouldBe` ["573", "294"]
      )