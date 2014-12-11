{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  User Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
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
  modUserAttr,
  )
import Tempuhs.Spoc.Init
  (
  initUser,
  initUserAttribute,
  )
import Tempuhs.Spoc.JSON
  (
  jsonKey,
  jsonSuccess,
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

userSpec :: Spec
-- | 'userSpec' runs the 'User' 'Spec's.
userSpec = do
  getSpec
  postSpec
  replaceSpec
  deleteSpec
  purgeSpec
  patchSpec
  attributesSpec

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
deleteSpec = rubbishSpec "user" initUser [(defaultUser, [] :: [()])]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "user" initUser

patchSpec :: Spec
patchSpec =
  describe "PATCH /users" $ do
    it "modifies an existing user and its attributes" $ do
      initUser
      patch "/users" "user=1&name=Abuser" >>= assertJSONOK (jsonKey 1)

attributesSpec :: Spec
attributesSpec =
  describe "PATCH /userAttributes" $ do
    it "inserts a user attribute with key 1"
      initUserAttribute
    it "modifies an existing user attribute" $ do
      initUserAttribute
      patch "/userAttributes" "user=1&key=name&value=new" >>=
        assertJSONOK (jsonKey 1)
      get "/users" >>= assertJSONOK [(defaultUser, [modUserAttr])]
    it "deletes an existing user attribute" $ do
      initUserAttribute
      patch "/userAttributes" "user=1&key=name" >>= assertJSONOK jsonSuccess
      get "/users" >>= assertJSONOK [(defaultUser, [] :: [()])]
    itReturnsMissingParam $ patch "/userAttributes" ""
