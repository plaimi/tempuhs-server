{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  POST Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module POST (
  postSpec,
  ) where

import Control.Monad
  (
  forM_,
  )
import Data.List
  (
  intercalate,
  subsequences,
  )
import qualified Data.Set as Z
import Test.Hspec
  (
  Spec,
  describe,
  )

import Spoc
  (
  it,
  itReturnsMissingParam,
  )
import Spoc.Assert
  (
  assertJSONError,
  assertJSONOK,
  )
import Spoc.Default
  (
  attributes,
  specifieds,
  )
import Spoc.Entity
  (
  modTimespanEntity,
  timespanEntity,
  timespansAttrs,
  timespansSpecs,
  timespansSpecsAttrs,
  )
import Spoc.Init
  (
  initAttribute,
  initClock,
  initModTimespan,
  initRole,
  initSubTimespan,
  initTimespanAttrs,
  initTimespanSpecs,
  initUser,
  )
import Spoc.JSON
  (
  jsonKey,
  jsonSuccess,
  )
import Spoc.Request
  (
  getTimespans,
  post,
  )

postSpec :: Spec
-- | 'postSpec' runs the POST 'Spec's.
postSpec = do
  clocksSpec
  timespansSpec
  attributesSpec
  usersSpec
  rolesSpec

clocksSpec :: Spec
clocksSpec = do
  describe "POST /clocks" $ do
    it "inserts a clock with key 1"
      initClock
    it "won't insert two clocks with the same name" $ do
      initClock
      post  "/clocks" "name=TT" >>= assertJSONError 500 "INTERNAL"
    it "modifies an existing clock" $ do
      initClock
      post "/clocks" "clock=1&name=TT2" >>= assertJSONOK (jsonKey 1)
      post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 2)
    itReturnsMissingParam $ post "/clocks" ""

timespansSpec :: Spec
timespansSpec = do
  describe "POST /timespans" $ do
    forM_ (subsequences . Z.toList $ specifieds) $
      \ss -> it ("inserts a timespan with key 1 specifying " ++
                  intercalate "/" ss) $ do
        initTimespanSpecs (Z.fromList ss)
        getTimespans (10, 42) >>=
          assertJSONOK (timespansSpecs (Z.fromList ss))
    it "successfully inserts a sub-timespan"
      initSubTimespan
    it "successfully inserts a timespan with attributes" $ do
      initTimespanAttrs attributes
      getTimespans (10, 42) >>= assertJSONOK (timespansAttrs attributes)
    it "modifies an existing timespan and its attributes" $ do
      initModTimespan
      getTimespans (10, 42) >>= assertJSONOK [modTimespanEntity]
    itReturnsMissingParam $ post "/timespans" ""

attributesSpec :: Spec
attributesSpec = do
  describe "POST /attributes" $ do
    it "inserts a timespan attribute with key 1"
      initAttribute
    it "modifies an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title&value=new" >>=
        assertJSONOK (jsonKey 1)
      getTimespans (10, 42) >>=
        assertJSONOK (timespansSpecsAttrs specifieds [("title", "new")])
    it "deletes an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title" >>= assertJSONOK jsonSuccess
      getTimespans (10, 42) >>=
        assertJSONOK [(timespanEntity specifieds, [] :: [()])]
    itReturnsMissingParam $ post "/attributes" ""

usersSpec :: Spec
usersSpec = do
  describe "POST /users" $ do
    it "inserts a user with key 1"
      initUser
    it "won't insert two users with the same name" $ do
      initUser
      post  "/users" "name=Luser" >>= assertJSONError 500 "INTERNAL"
    it "replaces an existing user" $ do
      initUser
      post "/users" "user=1&name=Joe" >>= assertJSONOK (jsonKey 1)
    itReturnsMissingParam $ post "/users" ""

rolesSpec :: Spec
rolesSpec = do
  describe "POST /roles" $ do
    it "inserts a role with key 1"
      initRole
    it "won't insert two roles with the same name and namespace" $ do
      initRole
      post "/roles" "name=Rulle&namespace=1" >>= assertJSONError 500 "INTERNAL"
    it "modifies an existing role's name" $ do
      initRole
      post "/roles" "role=1&name=Lulle" >>= assertJSONOK (jsonKey 1)
    it "modifies an existing role's namespace" $ do
      initRole
      post "/users" "user=2&name=Joe" >>= assertJSONOK (jsonKey 2)
      post "/roles" "role=1&namespace=2" >>= assertJSONOK (jsonKey 1)
    itReturnsMissingParam $ post "/roles" ""
