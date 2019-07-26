{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyTimeOfDaySpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Cursor.FuzzyTimeOfDay
import Cursor.FuzzyTimeOfDay.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @FuzzyTimeOfDayCursor
  genValidSpec @FuzzyTimeOfDayCursor
  describe "emptyFuzzyTimeOfDayCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids emptyFuzzyTimeOfDayCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyTimeOfDayCursorGuess (emptyFuzzyTimeOfDayCursor today) `shouldBe` Nothing
  describe "makeFuzzyTimeOfDayCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids makeFuzzyTimeOfDayCursor
    it "makes cursors that makes the guessing produce the given time" $
      forAllValid $ \d -> fuzzyTimeOfDayCursorGuess (makeFuzzyTimeOfDayCursor d) `shouldBe` Just d
  describe "rebuildFuzzyTimeOfDayCursor" $ do
    it "produces valid time of day" $ producesValidsOnValids rebuildFuzzyTimeOfDayCursor
  describe "fuzzyTimeOfDayCursorTextCursorL" $ lensSpecOnValid fuzzyTimeOfDayCursorTextCursorL
  describe "fuzzyTimeOfDayCursorGuess" $
    it "guesses a valid time of day" $ producesValidsOnValids fuzzyTimeOfDayCursorGuess
