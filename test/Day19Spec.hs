module Day19Spec (spec) where

import SpecHelper

testInput =
  "     |          \n\
  \     |  +--+    \n\
  \     A  |  C    \n\
  \ F---|----E|--+ \n\
  \     |  |  |  D \n\
  \     +B-+  +--+ \n\
  \"

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    day19 testInput `shouldBe` ["ABCDEF", "38"]

  it "Actual" $ do
    withFile
      "inputs/day19.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day19 actualInput `shouldBe` ["NDWHOYRUEA", "17540"]
      )