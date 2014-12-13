{- |
Module      :  $Header$
Description :  Property tests for timespans.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Props.Timespan where

import Test.Framework
  (
  Test,
  testGroup,
  )
import Test.Framework.Providers.QuickCheck2
  (
  testProperty,
  )

import Tempuhs.Props.Timespan.Laws
import Tempuhs.Props.Timespan.Props
import Tempuhs.Props.Instances ()

timespanLaws :: [Test]
timespanLaws = parentLaws
             ++ flexLaws

parentLaws :: [Test]
parentLaws = [testGroup "ParentLaws"
               [testProperty notDescParentLaw notDescParentTest
               ,testProperty notSelfParentLaw notSelfParentTest]]

flexLaws :: [Test]
flexLaws = [testGroup "FlexLaws"
             [testProperty isFlexLaw isFlexTest
             ,testProperty beginMinLaw beginMinTest
             ,testProperty endMaxLaw endMaxTest]]
