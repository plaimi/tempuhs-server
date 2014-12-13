{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Clock Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.Clock (
  clockSpec,
  ) where

import Test.Hspec
  (
  Spec,
  describe,
  )

import Tempuhs.Spoc
  (
  it,
  itReturnsMissingParam,
  )
import Tempuhs.Spoc.Assert
  (
  assertJSONError,
  assertJSONOK,
  )
import Tempuhs.Spoc.Entity
  (
  defaultClock,
  )
import Tempuhs.Spoc.Init
  (
  initClock,
  )
import Tempuhs.Spoc.JSON
  (
  jsonKey,
  )
import Tempuhs.Spoc.Request
  (
  get,
  patch,
  post,
  put,
  )
import Tempuhs.Tests.Requests.DELETE
  (
  rubbishSpec,
  unsafeRubbishSpec,
  )

clockSpec :: Spec
-- | 'clockSpec' runs the 'Clock' 'Spec's.
clockSpec = do
  getSpec
  postSpec
  replaceSpec
  deleteSpec
  purgeSpec
  patchSpec

getSpec :: Spec
getSpec =
  describe "GET /clocks" $ do
    it "initially returns []" $
      get "/clocks" >>= assertJSONOK ()
    it "returns clock after insertion" $ do
      initClock
      get "/clocks" >>= assertJSONOK [defaultClock]
    it "filters by name" $ do
      initClock
      get "/clocks?name=TT" >>= assertJSONOK [defaultClock]
      get "/clocks?name=NX" >>= assertJSONOK ()
    it "filters by key" $ do
      initClock
      get "/clocks?id=1" >>= assertJSONOK [defaultClock]
      get "/clocks?id=2" >>= assertJSONOK ()

postSpec :: Spec
postSpec =
  describe "POST /clocks" $ do
    it "inserts a clock with key 1"
      initClock
    it "won't insert two clocks with the same name" $ do
      initClock
      post  "/clocks" "name=TT" >>= assertJSONError 500 "INTERNAL"
    itReturnsMissingParam $ post "/clocks" ""

replaceSpec :: Spec
replaceSpec =
  describe "PUT /clocks" $ do
    it "replaces an existing clock" $ do
      initClock
      put "/clocks" "clock=1&name=TT2" >>= assertJSONOK (jsonKey 1)

deleteSpec :: Spec
deleteSpec = rubbishSpec "clock" initClock [defaultClock]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "clock" initClock

patchSpec :: Spec
patchSpec =
  describe "PATCH /clocks" $ do
    it "updates an existing clock" $ do
      initClock
      patch "/clocks" "clock=1&name=TT2" >>= assertJSONOK (jsonKey 1)
