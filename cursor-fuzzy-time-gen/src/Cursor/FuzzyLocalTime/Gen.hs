{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyLocalTime.Gen where

import Data.FuzzyTime.Types.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()

import Cursor.Text.Gen ()

import Cursor.FuzzyLocalTime

instance GenUnchecked FuzzyLocalTimeCursor

instance GenValid FuzzyLocalTimeCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
