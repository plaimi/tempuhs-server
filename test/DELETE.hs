{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  DELETE Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module DELETE (
  deleteSpec,
  ) where

import Test.Hspec
  (
  Spec,
  describe,
  )

import Spoc
  (
  it,
  )
import Spoc.Assert
  (
  assertJSON,
  assertJSONOK,
  assertStatus,
  )
import Spoc.Default
  (
  specifieds,
  )
import Spoc.Entity
  (
  defaultTimespans,
  rubbishP,
  )
import Spoc.Init
  (
  initTimespan,
  )
import Spoc.JSON
  (
  jsonSuccess,
  showJSON,
  )
import Spoc.Request
  (
  delete,
  get,
  )

deleteSpec :: Spec
-- | 'deleteSpec' runs the DELETE 'Spec's.
deleteSpec = timespansSpec

timespansSpec :: Spec
timespansSpec = do
  describe "DELETE /timespans" $ do
    it "rubbishes a timespan" $ do
      initTimespan specifieds
      delete "/timespans?timespan=1" >>= assertJSONOK jsonSuccess
    it "returns the rubbished timespan" $ do
      initTimespan specifieds
      delete "/timespans?timespan=1" >>= assertJSONOK jsonSuccess
      get "/timespans?rubbish=2000-01-01" >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON defaultTimespans)
                   r (rubbishP defaultTimespans)
