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
  post,
  )
import Tempuhs.Tests.Requests.DELETE
  (
  rubbishSpec,
  )

userSpec :: Spec
-- | 'userSpec' runs the 'User' 'Spec's.
userSpec = do
  postSpec
  getSpec
  deleteSpec

postSpec :: Spec
postSpec =
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

deleteSpec :: Spec
deleteSpec =  rubbishSpec "user" initUser [defaultUser]
