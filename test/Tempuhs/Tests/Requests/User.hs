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
  )
import Tempuhs.Spoc.Init
  (
  initUser,
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
      get "/users" >>= assertJSONOK [defaultUser]
    it "filters by name" $ do
      initUser
      get "/users?name=Luser" >>= assertJSONOK [defaultUser]
      get "/users?name=Ruser" >>= assertJSONOK ()

postSpec :: Spec
postSpec =
  describe "POST /users" $ do
    it "inserts a user with key 1"
      initUser
    it "won't insert two users with the same name" $ do
      initUser
      post  "/users" "name=Luser" >>= assertJSONError 500 "INTERNAL"
    itReturnsMissingParam $ post "/users" ""

replaceSpec :: Spec
replaceSpec =
  describe "PUT /users" $
    it "replaces an existing user" $ do
      initUser
      put "/users" "user=1&name=Joe" >>= assertJSONOK (jsonKey 1)

deleteSpec :: Spec
deleteSpec = rubbishSpec "user" initUser [defaultUser]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "user" initUser

patchSpec :: Spec
patchSpec =
  describe "PATCH /users" $ do
    it "modifies an existing user's name" $ do
      initUser
      patch "/users" "user=1&name=Abuser" >>= assertJSONOK (jsonKey 1)
