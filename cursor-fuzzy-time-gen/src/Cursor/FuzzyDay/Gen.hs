{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyDay.Gen where

import Cursor.FuzzyDay
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()

instance GenValid FuzzyDayCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
