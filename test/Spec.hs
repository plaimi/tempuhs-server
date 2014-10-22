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

import DELETE
  (
  deleteSpec,
  )
import GET
  (
  getSpec,
  )
import POST
  (
  postSpec,
  )

spec :: Spec
-- | 'spec' runs all the 'Spec's for the tempuhs web server application.
spec = postSpec >> getSpec >> deleteSpec

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
