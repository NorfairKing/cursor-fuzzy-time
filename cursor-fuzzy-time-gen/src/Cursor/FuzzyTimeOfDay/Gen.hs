{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyTimeOfDay.Gen where

import Cursor.FuzzyTimeOfDay
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()

instance GenValid FuzzyTimeOfDayCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
