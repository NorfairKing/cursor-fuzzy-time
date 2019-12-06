{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion

import Data.GenValidity.Criterion

import Cursor.FuzzyDay
import Cursor.FuzzyDay.Gen ()
import Cursor.FuzzyLocalTime
import Cursor.FuzzyLocalTime.Gen ()
import Cursor.FuzzyTimeOfDay
import Cursor.FuzzyTimeOfDay.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @FuzzyDayCursor
    , genValidBench @FuzzyLocalTimeCursor
    , genValidBench @FuzzyTimeOfDayCursor
    ]
