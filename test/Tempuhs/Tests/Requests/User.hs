{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  User Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.User (
  userSpec,
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
  defaultUser,
  defaultUserWithAttrs,
  )
import Tempuhs.Spoc.Init
  (
  initUser,
  initUserAttribute,
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
  attributesRubbishSpec,
  unsafeRubbishSpec,
  )

userSpec :: Spec
-- | 'userSpec' runs the 'User' 'Spec's.
userSpec = do
  getSpec
  postSpec
  replaceSpec
  deleteSpec
  purgeSpec
  patchSpec

getSpec :: Spec
getSpec =
  describe "GET /users" $ do
    it "initially returns []" $
      get "/users" >>= assertJSONOK ()
    it "returns user after insertion" $ do
      initUser
      get "/users" >>= assertJSONOK [(defaultUser, [] :: [()])]
    it "filters by name" $ do
      initUser
      get "/users?name=Luser" >>= assertJSONOK [(defaultUser, [] :: [()])]
      get "/users?name=Ruser" >>= assertJSONOK ()
    it "returns associated user attributes" $ do
      initUserAttribute
      get "/users" >>= assertJSONOK [defaultUserWithAttrs]

postSpec :: Spec
postSpec =
  describe "POST /users" $ do
    it "inserts a user with key 1"
      initUser
    it "won't insert two users with the same name" $ do
      initUser
      post  "/users" "name=Luser" >>= assertJSONError 500 "INTERNAL"
    it "successfully inserts a user with attributes" $ do
      initUserAttribute
      get "/users" >>= assertJSONOK [defaultUserWithAttrs]
    itReturnsMissingParam $ post "/users" ""

replaceSpec :: Spec
replaceSpec =
  describe "PUT /users" $
    it "replaces an existing user" $ do
      initUser
      put "/users" "user=1&name=Joe" >>= assertJSONOK (jsonKey 1)

deleteSpec :: Spec
deleteSpec = attributesRubbishSpec "user" initUserAttribute [defaultUserWithAttrs]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "user" initUser

patchSpec :: Spec
patchSpec =
  describe "PATCH /users" $ do
    it "modifies an existing user and its attributes" $ do
      initUser
      patch "/users" "user=1&name=Abuser" >>= assertJSONOK (jsonKey 1)
    it "rubbishes an existing user attribute" $ do
      initUserAttribute
      patch "/users" "user=1&name_=" >>= assertJSONOK (jsonKey 1)
    it "returns the rubbished user attribute" $ do
      initUserAttribute
      patch "/users" "user=1&name_=" >>= assertJSONOK (jsonKey 1)
    it "deletes a rubbished user attribute" $ do
      initUserAttribute
      patch "/users" "user=1&name_=" >> patch "/users" "user=1&name_"
        >>= assertJSONOK (jsonKey 1)
