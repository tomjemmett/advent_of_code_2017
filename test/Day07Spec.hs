module Day07Spec (spec) where

import SpecHelper

testInput =
  "pbga (66)\n\
  \xhth (57)\n\
  \ebii (61)\n\
  \havc (66)\n\
  \ktlj (57)\n\
  \fwft (72) -> ktlj, cntj, xhth\n\
  \qoyq (66)\n\
  \padx (45) -> pbga, havc, qoyq\n\
  \tknk (41) -> ugml, padx, fwft\n\
  \jptl (61)\n\
  \ugml (68) -> gyxo, ebii, jptl\n\
  \gyxo (61)\n\
  \cntj (57)"

spec :: Spec
spec = describe "Day 7" $ do
  it "Sample" $ do
    day07 testInput `shouldBe` ["tknk", "60"]

  it "Actual" $ do
    withFile
      "inputs/day07.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day07 actualInput `shouldBe` ["dgoocsw", "1275"]
      )