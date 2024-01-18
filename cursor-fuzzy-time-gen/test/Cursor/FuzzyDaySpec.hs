{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyDaySpec where

import Cursor.FuzzyDay
import Cursor.FuzzyDay.Gen ()
import Data.Int
import Data.Time
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  eqSpec @FuzzyDayCursor
  genValidSpec @FuzzyDayCursor
  describe "emptyFuzzyDayCursor" $ do
    it "produces valid cursors" $ producesValid emptyFuzzyDayCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyDayCursorGuessForwards (emptyFuzzyDayCursor today) `shouldBe` Nothing
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyDayCursorGuessBackwards (emptyFuzzyDayCursor today) `shouldBe` Nothing

  describe "makeFuzzyDayCursor" $ do
    it "produces valid cursors" $ producesValid makeFuzzyDayCursor
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyDayCursorGuessForwards (makeFuzzyDayCursor d) `shouldBe` Just d
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyDayCursorGuessBackwards (makeFuzzyDayCursor d) `shouldBe` Just d

  describe "rebuildFuzzyDayCursorForwards" $ do
    it "produces valid days" $ producesValid rebuildFuzzyDayCursorForwards
  describe "rebuildFuzzyDayCursorBackwards" $ do
    it "produces valid days" $ producesValid rebuildFuzzyDayCursorBackwards

  describe "fuzzyDayCursorTextCursorL" $
    lensSpec fuzzyDayCursorTextCursorL

  describe "fuzzyDayCursorGuessForwards" $
    it "guesses a valid day" $
      producesValid fuzzyDayCursorGuessForwards
  describe "fuzzyDayCursorGuessBackwards" $
    it "guesses a valid day" $
      producesValid fuzzyDayCursorGuessBackwards
