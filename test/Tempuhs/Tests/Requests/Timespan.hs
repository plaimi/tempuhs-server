{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Timespan Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.Timespan (
  timespanSpec,
  ) where

import qualified Data.ByteString as B
import Control.Monad
  (
  (>=>),
  forM,
  forM_,
  guard,
  join,
  replicateM,
  )
import Data.Functor
  (
  (<$>),
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

import Tempuhs.Spoc
  (
  it,
  itReturnsMissingParam,
  )
import Tempuhs.Spoc.Assert
  (
  assertJSONOK,
  )
import Tempuhs.Spoc.Default
  (
  attributes,
  specifieds,
  )
import Tempuhs.Spoc.Entity
  (
  defaultTimespans,
  modTimespanEntity,
  specialTimespan,
  timespansAttrs,
  timespansSpecs,
  )
import Tempuhs.Spoc.Init
  (
  initClock,
  initDefaultTimespan,
  initModTimespan,
  initSubTimespan,
  initTimespanAttrs,
  initTimespanSpecs,
  )
import Tempuhs.Spoc.JSON
  (
  jsonKey,
  )
import Tempuhs.Spoc.Request
  (
  buildQuery,
  get,
  getTimespans,
  patch,
  post,
  put,
  )
import Tempuhs.Tests.Requests.DELETE
  (
  attributesRubbishSpec,
  unsafeRubbishSpec,
  )

timespanSpec :: Spec
-- | 'timespanSpec' runs the 'Timespan' 'Spec's.
timespanSpec = do
  getSpec
  postSpec
  replaceSpec
  deleteSpec
  purgeSpec
  patchSpec

getSpec :: Spec
getSpec =
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
      forM_ ([("begin" :: String, x) | x <- begins] ++
             [("end",             x) | x <- ends]) $ \(p, v) ->
        get (B.append "/timespans?" $ buildQuery [(p, show v)]) >>=
          assertJSONOK defaultTimespans
    it "returns [] for views that don't intersect any timespan" $ do
      initDefaultTimespan
      forM_ [(0, 9), (43, 50)] $ getTimespans >=> assertJSONOK ()
      forM_ ["begin=43", "end=9"] $
        get . B.append "/timespans?" >=> assertJSONOK ()
    it "returns associated timespan attributes" $ do
      initTimespanAttrs attributes
      getTimespans (10, 42) >>=
        assertJSONOK (timespansAttrs attributes)
    it "filters on parent" $ do
      initDefaultTimespan
      initSubTimespan 2 1
      get "/timespans?parent=1" >>=
        assertJSONOK [(specialTimespan 2 (Just 1), [] :: [()])]
    it "returns [] asking for rubbish after inserting useful timespans" $ do
      initDefaultTimespan
      get "/timespans?rubbish=2000-01-01" >>= assertJSONOK ()
    it "returns a timespan and all of its descendants" $ do
      initDefaultTimespan
      initSubTimespan 2 1
      initSubTimespan 3 2
      get "/timespans?id=1&descendants=Infinity" >>=
        assertJSONOK (defaultTimespans ++ (specialTimespan 2 (Just 1), []) :
                                          [(specialTimespan 3 (Just 2), [])])
    it "returns a timespan and 1 level of descendants" $ do
      initDefaultTimespan
      initSubTimespan 2 1
      initSubTimespan 3 2
      get "/timespans?id=1&descendants=1" >>=
        assertJSONOK (defaultTimespans ++ [(specialTimespan 2 (Just 1), [])])
    it "filters on attributes" $ do
      initDefaultTimespan
      let qs :: String -> String -> B.ByteString
          qs k v = B.append "/timespans?" $ buildQuery [(k, v)]
          go x l = forM_ [qs k v | (k, vs) <- l, v <- vs] $
                         get >=> assertJSONOK x
      go ()
         [("nx_",  ["", "fu"]),       ("nx_like",  ["%"])
         ,("foo_", ["%", "f", "Fu"]), ("foo_like", [""])]
      go defaultTimespans
         [("foo_",     ["fu"])
         ,("foo_like", ["fu", "%", "%u", "f%", "%fu%", "f%u"])]
    it "only returns timespans that match all attribute filters" $ do
      initDefaultTimespan
      let foos   = [("like", "%"), ("", "fu")]
          bars   = [("like", "%"), ("", "baz")]
          qs :: [(String, (String, String))] -> B.ByteString
          qs     = B.append "/timespans?" . buildQuery .
                     map (\(k, (o, v)) -> (k ++ "_" ++ o, v))
          go x l = get (qs . join . forM l $ uncurry (fmap . (,))) >>=
                     assertJSONOK x
      go ()
         [("foo", foos), ("nx", foos)]
      go defaultTimespans
         [("foo", foos), ("bar", bars)]
    it "can filter on the same attribute multiple times" $ do
      initDefaultTimespan
      let pos = subsequences ["%", "baz", "b%z", "%baz%", "%b%z%"]
          neg = do
            a <- pos
            b <- ["", "b", "%bar%", "%x%"]
            [b : a, a ++ [b]]
          qs :: [String] -> B.ByteString
          qs  = B.append "/timespans?" . buildQuery .
                  map ((,) ("bar_like" :: String))
      forM_ (zip pos (repeat defaultTimespans) ++ zip neg (repeat [])) $
        \(a, b) -> get (qs a) >>= assertJSONOK b

postSpec :: Spec
postSpec =
  describe "POST /timespans" $ do
    forM_ (subsequences . Z.toList $ specifieds) $
      \ss -> it ("inserts a timespan with key 1 specifying " ++
                  intercalate "/" ss) $ do
        initTimespanSpecs (Z.fromList ss)
        getTimespans (10, 42) >>=
          assertJSONOK (timespansSpecs (Z.fromList ss))
    it "successfully inserts a sub-timespan" $ do
      initDefaultTimespan
      initSubTimespan 2 1
    it "successfully inserts a timespan with attributes" $ do
      initTimespanAttrs attributes
      getTimespans (10, 42) >>= assertJSONOK (timespansAttrs attributes)
    itReturnsMissingParam $ post "/timespans" ""

replaceSpec :: Spec
replaceSpec =
  describe "PUT /timespans" $
    it "replaces a timespan" $ do
      initDefaultTimespan
      put "/timespans" "?timespan=1&clock=TT&beginMin=10.0"
        >>= assertJSONOK (jsonKey 1)

deleteSpec :: Spec
deleteSpec = attributesRubbishSpec "timespan" initDefaultTimespan
                                              defaultTimespans

purgeSpec :: Spec
purgeSpec = unsafeRubbishSpec "timespan" initDefaultTimespan

patchSpec :: Spec
patchSpec =
  describe "PATCH /timespans" $ do
    it "modifies an existing timespan and its attributes" $ do
      initModTimespan
      getTimespans (10, 42) >>= assertJSONOK [modTimespanEntity]
    it "rubbishes an existing timespan attribute" $ do
      initTimespanAttrs attributes
      patch "/timespans" "timespan=1&title_=" >>= assertJSONOK (jsonKey 1)
    it "returns the rubbished user attribute" $ do
      initTimespanAttrs attributes
      patch "/timespans" "timespan=1&title_=" >>= assertJSONOK (jsonKey 1)
    it "deletes a rubbished user attribute" $ do
      initTimespanAttrs attributes
      last <$> replicateM 2 (patch "/timespans" "timespan=1&title_=")
        >>= assertJSONOK (jsonKey 1)
