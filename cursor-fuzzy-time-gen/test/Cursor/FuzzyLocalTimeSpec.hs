{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyLocalTimeSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Data.FuzzyTime

import Cursor.FuzzyLocalTime
import Cursor.FuzzyLocalTime.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @FuzzyLocalTimeCursor
  genValidSpec @FuzzyLocalTimeCursor
  describe "emptyFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids emptyFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyLocalTimeCursorGuess (emptyFuzzyLocalTimeCursor today) `shouldBe` Nothing
  describe "makeFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids makeFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce the given time" $
      forAllValid $ \d ->
        fuzzyLocalTimeCursorGuess (makeFuzzyLocalTimeCursor d) `shouldBe` Just (BothTimeAndDay d)
  describe "rebuildFuzzyLocalTimeCursor" $ do
    it "produces valid time of day" $ producesValidsOnValids rebuildFuzzyLocalTimeCursor
  describe "fuzzyLocalTimeCursorTextCursorL" $ lensSpecOnValid fuzzyLocalTimeCursorTextCursorL
  describe "fuzzyLocalTimeCursorGuess" $
    it "guesses a valid time of day" $ producesValidsOnValids fuzzyLocalTimeCursorGuess
