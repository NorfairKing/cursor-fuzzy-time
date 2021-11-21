{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyTimeOfDaySpec where

import Cursor.FuzzyTimeOfDay
import Cursor.FuzzyTimeOfDay.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  eqSpec @FuzzyTimeOfDayCursor
  genValidSpec @FuzzyTimeOfDayCursor
  describe "emptyFuzzyTimeOfDayCursor" $ do
    it "produces valid cursors" $
      producesValid emptyFuzzyTimeOfDayCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyTimeOfDayCursorGuess (emptyFuzzyTimeOfDayCursor today)
          `shouldBe` Nothing
  describe "makeFuzzyTimeOfDayCursor" $ do
    it "produces valid cursors" $
      producesValid makeFuzzyTimeOfDayCursor
    it "makes cursors that makes the guessing produce the given time" $
      forAllValid $ \d ->
        fuzzyTimeOfDayCursorGuess (makeFuzzyTimeOfDayCursor d) `shouldBe` Just d
  describe "rebuildFuzzyTimeOfDayCursor" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyTimeOfDayCursor
  describe "fuzzyTimeOfDayCursorTextCursorL" $
    lensSpec fuzzyTimeOfDayCursorTextCursorL
  describe "fuzzyTimeOfDayCursorGuess" $
    it "guesses a valid time of day" $
      producesValid fuzzyTimeOfDayCursorGuess
