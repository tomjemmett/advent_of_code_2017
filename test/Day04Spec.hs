module Day04Spec (spec) where

import SpecHelper

testInput =
  "aa bb cc dd ee\n\
  \aa bb cc dd aa\n\
  \aa bb cc dd aaa\n\
  \abcde fghij\n\
  \abcde xyz ecdab\n\
  \a ab abc abd abf abj\n\
  \iiii oiii ooii oooi oooo\n\
  \oiii ioii iioi iiio"

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    day04 testInput `shouldBe` ["7", "5"]

  it "Actual" $ do
    withFile
      "inputs/day04.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day04 actualInput `shouldBe` ["337", "231"]
      )