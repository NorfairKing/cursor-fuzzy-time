{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyTimeOfDay.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()

import Cursor.Text.Gen ()

import Cursor.FuzzyTimeOfDay

instance GenUnchecked FuzzyTimeOfDayCursor

instance GenValid FuzzyTimeOfDayCursor
