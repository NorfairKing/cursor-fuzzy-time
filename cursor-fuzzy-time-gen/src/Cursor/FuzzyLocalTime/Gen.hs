{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyLocalTime.Gen where

import Cursor.FuzzyLocalTime
import Cursor.Text.Gen ()
import Data.FuzzyTime.Types.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()

instance GenValid FuzzyLocalTimeCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
