{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Permissionset Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.Permissionset (
  permissionsetSpec,
  ) where

import Control.Monad
  (
  forM_,
  )
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
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
  assertJSONError,
  assertJSONOK,
  )
import Tempuhs.Spoc.Entity
  (
  defaultPermissionset,
  )
import Tempuhs.Spoc.Init
  (
  initPermissionset,
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
  )
import Tempuhs.Tests.Requests.DELETE
  (
  rubbishSpec,
  unsafeRubbishSpec,
  )

permissionsetSpec :: Spec
-- | 'permissionsetSpec' runs the 'Permissionset' 'Spec's.
permissionsetSpec = do
  getSpec
  postSpec
  deleteSpec
  purgeSpec
  patchSpec

getSpec :: Spec
getSpec =
  describe "GET /permissionsets" $ do
    it "initially returns []" $
      get "/permissionsets" >>= assertJSONOK ()
    it "returns a permissionset set after insertion" $ do
      initPermissionset
      get "/permissionsets" >>= assertJSONOK [defaultPermissionset]
    it "filters by timespan" $ do
      initPermissionset
      get "/permissionsets?timespan=1" >>= assertJSONOK [defaultPermissionset]
      get "/permissionsets?timespan=2" >>= assertJSONOK ()
    it "filters by role" $ do
      initPermissionset
      get "/permissionsets?role=1" >>= assertJSONOK [defaultPermissionset]
      get "/permissionsets?role=2" >>= assertJSONOK ()

postSpec :: Spec
postSpec =
  describe "POST /permissionsets" $ do
    it ("inserts a timespan and role and gives the role all permissions " ++
        "for the timespan -- the permissionset has key 1")
      initPermissionset
    it "won't insert two permissionsets for the same timespan and role" $ do
      initPermissionset
      post "/permissionsets"
           "timespan=1&role=1&own=True&read=True&write=True"
         >>= assertJSONError 500 "INTERNAL"

deleteSpec :: Spec
deleteSpec = rubbishSpec "permissionset" initPermissionset
                                         [defaultPermissionset]

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "permissionset" initPermissionset

patchSpec :: Spec
patchSpec =
  describe "PATCH /permissionsets" $ do
    forM_ ["own", "read", "write"] $ \ps ->
          it ("modifies an existing permissionset's " ++ ps) $ do
            initPermissionset
            patch "/permissionsets" ("permissionset=1&" `LB.append`
                                     LB8.pack ps `LB.append` "=False")
              >>= assertJSONOK (jsonKey 1)
