{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Specs for tempuhs metadata.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.Meta (
  metaSpec,
  ) where

import Test.Hspec
  (
  Spec,
  describe,
  )

import Tempuhs.Spoc
  (
  it,
  )
import Tempuhs.Spoc.Assert
  (
  assertContentType,
  )
import Tempuhs.Spoc.Request
  (
  get,
  )

metaSpec :: Spec
-- | 'metaSpec' runs the 'Spec's related to tempuhs metadata.
metaSpec =
  srcSpec

srcSpec :: Spec
srcSpec =
  describe "GET /src" $ do
    it "return a tar.gz file, hopefully with the source code of tempuhs" $
      get "/src" >>= assertContentType "application/x-gzip"
