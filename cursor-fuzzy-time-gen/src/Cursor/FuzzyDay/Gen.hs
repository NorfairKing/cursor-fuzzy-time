{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FuzzyDay.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()

import Cursor.Text.Gen ()

import Cursor.FuzzyDay

instance GenUnchecked FuzzyDayCursor

instance GenValid FuzzyDayCursor
