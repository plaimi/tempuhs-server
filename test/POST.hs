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
import qualified Data.Text as T
import Test.Hspec
  (
  Spec,
  describe,
  )

import Plailude
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
  attributeEntity,
  defaultTimespans,
  firstTimespans,
  modTimespanEntity,
  )
import Spoc.Init
  (
  initAttribute,
  initClock,
  initModTimespan,
  initSubTimespan,
  initTimespan,
  initTimespanWithAttrs,
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
postSpec = clocksSpec >> timespansSpec >> attributesSpec

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
        initTimespan $ Z.fromList ss
        getTimespans (10, 42) >>=
          assertJSONOK (firstTimespans (Z.fromList ss) [])
    it "successfully inserts a sub-timespan"
      initSubTimespan
    it "successfully inserts a timespan with attributes" $ do
      initTimespanWithAttrs attributes
      getTimespans (10, 42) >>=
        assertJSONOK (firstTimespans Z.empty
          [attributeEntity i 1 k v
          | (i, (k, v)) <- zip [1 .. ] $ map (both T.pack) attributes])
    it "modifies an existing timespan" $ do
      initModTimespan
      getTimespans (10, 42) >>=
        assertJSONOK [(modTimespanEntity, [] :: [()])]
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
        assertJSONOK (firstTimespans specifieds
                                     [attributeEntity 1 1 "title" "new"])
    it "deletes an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title" >>= assertJSONOK jsonSuccess
      getTimespans (10, 42) >>= assertJSONOK defaultTimespans
    itReturnsMissingParam $ post "/attributes" ""
