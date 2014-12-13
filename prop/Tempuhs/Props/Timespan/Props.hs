{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Property tests for timespans.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Props.Timespan.Props where

import Test.QuickCheck
  (
  NonNegative,
  NonEmptyList,
  Property,
  (===),
  getNonEmpty,
  getNonNegative,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  )
import Tempuhs.Server.Laws.Props
  (
  beginMinProp,
  endMaxProp,
  isFlexProp,
  parentCycleProp,
  )

isFlexTest :: Clock -> Property
isFlexTest c = (clockName c == "<~>") === isFlexProp c

beginMinTest :: NonEmptyList ProperTime -> Property
beginMinTest bms =
  beginMinProp (map f (getNonEmpty bms)) === minimum (getNonEmpty bms)
  where f bm = Timespan Nothing (mkKey 1) bm 0 0 0 0 Nothing

endMaxTest :: NonEmptyList ProperTime -> Property
endMaxTest ems =
  endMaxProp (map f (getNonEmpty ems)) === maximum (getNonEmpty ems)
  where f em = Timespan Nothing (mkKey 1) 0 0 0 em 0 Nothing

notDescParentTest :: Integer -> [NonNegative Integer] -> Property
notDescParentTest t ps = not (t `elem` qs) === parentCycleProp t qs
  where qs = map getNonNegative ps

notSelfParentTest :: NonNegative Integer -> Property
notSelfParentTest t = parentCycleProp u [u] === False
  where u = getNonNegative t
