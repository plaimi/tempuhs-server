{- |
Module      :  $Header$
Description :  Property tests for tempuhs-server
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main (main) where

import Test.Framework
  (
  Test,
  defaultMain,
  )

import Tempuhs.Props.Timespan
  (
  timespanLaws,
  )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = timespanLaws
