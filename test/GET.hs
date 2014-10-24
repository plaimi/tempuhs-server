{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  GET Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module GET (
  getSpec,
  ) where

import Control.Monad
  (
  (>=>),
  forM_,
  guard,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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
  assertJSONOK,
  )
import Spoc.Default
  (
  specifieds,
  )
import Spoc.Entity
  (
  clockEntity,
  defaultTimespans,
  specialTimespan,
  timespansSpecsAttrs,
  )
import Spoc.Init
  (
  initAttribute,
  initClock,
  initDefaultTimespan,
  initSubTimespan,
  )
import Spoc.Request
  (
  get,
  getTimespans,
  )

getSpec :: Spec
-- | 'getSpec' runs the GET 'Spec's.
getSpec = clockSpec >> timespansSpec

clockSpec :: Spec
clockSpec = do
  describe "GET /clocks" $ do
    it "initially returns []" $
      get "/clocks" >>= assertJSONOK ()
    it "returns clock after insertion" $ do
      initClock
      get "/clocks" >>= assertJSONOK [clockEntity 1 "TT"]
    it "filters by name" $ do
      initClock
      get "/clocks?name=TT" >>= assertJSONOK [clockEntity 1 "TT"]
      get "/clocks?name=NX" >>= assertJSONOK ()
    it "filters by key" $ do
      initClock
      get "/clocks?id=1" >>= assertJSONOK [clockEntity 1 "TT"]
      get "/clocks?id=2" >>= assertJSONOK ()

timespansSpec :: Spec
timespansSpec = do
  describe "GET /timespans" $ do
    it "initially returns []" $
      get "/timespans" >>= assertJSONOK ()
    it "initially returns [] for an existing clock" $ do
      initClock
      get "/timespans?clock=TT&begin=0&end=0" >>= assertJSONOK ()
    it "returns all timespans that touch or intersect the view" $ do
      initDefaultTimespan
      let
        begins = [9, 10, 11, 41, 42]
        ends   = [10, 11, 41, 42, 43]
        ranges = do
          begin <- begins
          end   <- ends
          guard  (begin <= end)
          return (begin, end)
      forM_ ranges $
        getTimespans >=> assertJSONOK defaultTimespans
      forM_ ([("begin", x) | x <- begins] ++
             [("end",   x) | x <- ends]) $ \(p, v) ->
        get (B.concat ["/timespans?", p, "=", B8.pack $ show v]) >>=
          assertJSONOK defaultTimespans
    it "returns [] for views that don't intersect any timespan" $ do
      initDefaultTimespan
      forM_ [(0, 9), (43, 50)] $ getTimespans >=> assertJSONOK ()
      forM_ ["begin=43", "end=9"] $
        get . B.append "/timespans?" >=> assertJSONOK ()
    it "returns associated timespan attributes" $ do
      initAttribute
      getTimespans (10, 42) >>=
        assertJSONOK (timespansSpecsAttrs specifieds [("title", "test")])
    it "filters on parent" $ do
      initSubTimespan
      get "/timespans?parent=1" >>=
        assertJSONOK [(specialTimespan (Just 1), [] :: [()])]
    it "returns [] asking for rubbish after inserting useful timespans" $ do
      initDefaultTimespan
      get "/timespans?rubbish=2000-01-01" >>= assertJSONOK ()
