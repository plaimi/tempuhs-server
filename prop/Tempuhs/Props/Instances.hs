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
import Test.QuickCheck
  (
  Arbitrary,
  arbitrary,
  )
import Test.QuickCheck.Instances ()
import Tempuhs.Chronology

instance Arbitrary Clock where
  arbitrary = Clock <$> arbitrary <*> arbitrary
