{-# LANGUAGE    FlexibleInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

{- |
Module      :  $Header$
Description :  Necessary instances for the property tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Props.Instances where

import Control.Applicative
  (
  (<$>),
  (<*>),
  )
import Database.Persist
  (
  Key,
  )
import Test.QuickCheck
  (
  Arbitrary,
  arbitrary,
  )
import Test.QuickCheck.Instances ()

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  )

instance Arbitrary Clock where
  arbitrary = Clock <$> arbitrary <*> arbitrary

instance Arbitrary Timespan where
  arbitrary = Timespan <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Key Clock) where
  arbitrary = mkKey <$> arbitrary

instance Arbitrary (Key Timespan) where
  arbitrary = mkKey <$> arbitrary
