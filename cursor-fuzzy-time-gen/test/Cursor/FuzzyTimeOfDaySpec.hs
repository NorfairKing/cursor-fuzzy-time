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
        fuzzyTimeOfDayCursorGuessForwards (emptyFuzzyTimeOfDayCursor today)
          `shouldBe` Nothing
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyTimeOfDayCursorGuessBackwards (emptyFuzzyTimeOfDayCursor today)
          `shouldBe` Nothing

  describe "makeFuzzyTimeOfDayCursor" $ do
    it "produces valid cursors" $
      producesValid makeFuzzyTimeOfDayCursor
    it "makes cursors that makes the guessing produce the given time" $
      forAllValid $ \d ->
        fuzzyTimeOfDayCursorGuessForwards (makeFuzzyTimeOfDayCursor d) `shouldBe` Just d
    it "makes cursors that makes the guessing produce the given time" $
      forAllValid $ \d ->
        fuzzyTimeOfDayCursorGuessBackwards (makeFuzzyTimeOfDayCursor d) `shouldBe` Just d

  describe "rebuildFuzzyTimeOfDayCursorForwards" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyTimeOfDayCursorForwards

  describe "rebuildFuzzyTimeOfDayCursorBackwards" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyTimeOfDayCursorBackwards

  describe "fuzzyTimeOfDayCursorTextCursorL" $
    lensSpec fuzzyTimeOfDayCursorTextCursorL

  describe "fuzzyTimeOfDayCursorGuessForwards" $
    it "guesses a valid time of day" $
      producesValid fuzzyTimeOfDayCursorGuessForwards
  describe "fuzzyTimeOfDayCursorGuessBackwards" $
    it "guesses a valid time of day" $
      producesValid fuzzyTimeOfDayCursorGuessBackwards
