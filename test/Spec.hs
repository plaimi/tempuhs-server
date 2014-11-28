{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Runs the Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main where

import Test.Hspec
  (
  Spec,
  hspec,
  )

import Tempuhs.Tests.Requests.Clock
  (
  clockSpec,
  )
import Tempuhs.Tests.Requests.Permissionset
  (
  permissionsetSpec,
  )
import Tempuhs.Tests.Requests.Role
  (
  roleSpec,
  )
import Tempuhs.Tests.Requests.Timespan
  (
  timespanSpec,
  )
import Tempuhs.Tests.Requests.User
  (
  userSpec,
  )

spec :: Spec
-- | 'spec' runs all the 'Spec's for the tempuhs web server application.
spec = do
  timespanSpec
  clockSpec
  roleSpec
  userSpec
  permissionsetSpec

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
