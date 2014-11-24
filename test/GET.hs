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
  forM,
  forM_,
  guard,
  join,
  )
import qualified Data.ByteString as B
import Data.List
  (
  subsequences,
  )
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
  defaultRole,
  defaultTimespans,
  defaultUser,
  specialTimespan,
  timespansSpecsAttrs,
  )
import Spoc.Init
  (
  initAttribute,
  initClock,
  initDefaultTimespan,
  initRole,
  initSubTimespan,
  initUser,
  )
import Spoc.Request
  (
  buildQuery,
  get,
  getTimespans,
  )

getSpec :: Spec
-- | 'getSpec' runs the GET 'Spec's.
getSpec = do
  clockSpec
  timespansSpec
  usersSpec
  rolesSpec

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

usersSpec :: Spec
usersSpec = do
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

rolesSpec :: Spec
rolesSpec = do
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
