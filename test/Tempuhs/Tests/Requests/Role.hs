{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Role Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-}

module Tempuhs.Tests.Requests.Role (
  roleSpec,
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
  defaultRole,
  )
import Tempuhs.Spoc.Init
  (
  initRole,
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

roleSpec :: Spec
-- | 'roleSpec' runs the 'Role' 'Spec's.
roleSpec = do
  getSpec
  postSpec
  replaceSpec
  deleteSpec
  purgeSpec
  patchSpec

getSpec :: Spec
getSpec =
  describe "GET /roles" $ do
    it "initially returns []" $
      get "/roles" >>= assertJSONOK ()
    it "returns role after insertion" $ do
      initRole
      get "/roles" >>= assertJSONOK [defaultRole]
    it "filters by name" $ do
      initRole
      get "/roles?name=Rulle" >>= assertJSONOK [defaultRole]
      get "/roles?name=Rolle" >>= assertJSONOK ()
    it "filters by namespace" $ do
      initRole
      get "/roles?namespace=1" >>= assertJSONOK [defaultRole]
      get "/roles?namespace=2" >>= assertJSONOK ()

postSpec :: Spec
postSpec =
  describe "POST /roles" $ do
    it "inserts a role with key 1"
      initRole
    it "won't insert two roles with the same name and namespace" $ do
      initRole
      post "/roles" "name=Rulle&namespace=1" >>= assertJSONError 500 "INTERNAL"
    itReturnsMissingParam $ post "/roles" ""

replaceSpec :: Spec
replaceSpec =
  describe "PUT /roles" $
    it "replaces an existing role" $ do
      initRole
      post "/users" "name=Lulle" >>= assertJSONOK (jsonKey 2)
      put "/roles" "role=1&name=Lulle&namespace=2" >>= assertJSONOK (jsonKey 1)

deleteSpec :: Spec
deleteSpec = rubbishSpec "role" initRole [defaultRole]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "role" initRole

patchSpec :: Spec
patchSpec =
  describe "PATCH /roles" $ do
    it "modifies an existing role's name" $ do
      initRole
      patch "/roles" "role=1&name=Lulle" >>= assertJSONOK (jsonKey 1)
    it "modifies an existing role's namespace" $ do
      initRole
      patch "/users" "user=2&name=Joe" >>= assertJSONOK (jsonKey 2)
      patch "/roles" "role=1&namespace=2" >>= assertJSONOK (jsonKey 1)
