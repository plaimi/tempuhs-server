{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Spec for the tempuhs web server application
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main where

import Control.Monad
  ((>=>)
  ,forM_)
import Control.Monad.IO.Class
  (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.Persist.Sqlite
  (withSqlitePool)
import Network.HTTP.Types
  (statusCode)
import Network.Wai
  (Request
  ,requestHeaders
  ,requestMethod)
import Network.Wai.Test
  (Session
  ,SResponse
  ,SRequest (SRequest)
  ,defaultRequest
  ,request
  ,runSession
  ,setPath
  ,simpleBody
  ,simpleStatus
  ,srequest)
import System.IO
  (stderr)
import System.IO.Silently
  (hSilence)
import Test.HUnit
  (assertBool)
import Test.Hspec
  (Spec
  ,describe
  ,hspec)
import qualified Test.Hspec as HS
import Web.Scotty
  (scottyApp)

import Tempuhs.Server

formPostRequest :: Request
-- | 'formPostRequest' is a blank POST request for use with URL-encoded form
-- data as the body.
formPostRequest = defaultRequest
  { requestMethod = "POST"
  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]}

get :: B.ByteString -> Session SResponse
-- | 'get' makes a GET 'request' with the given path.
get = request . setPath defaultRequest

post :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'post' makes a POST request makes a POST 'request' with the given path
-- and URL-encoded form data body.
post p b = srequest $ SRequest (setPath formPostRequest p) b

assertStatus :: Int -> SResponse -> Session ()
-- | 'assertStatus' checks that the status code of a response matches the
-- expected value.
assertStatus code r = liftIO $ assertBool msg $ code == status
  where
    msg     = "Expected status code " ++ show code ++
              ", but received "       ++ show status
    status  = statusCode $ simpleStatus r

assertBody :: L.ByteString -> SResponse -> Session ()
-- | 'assertBody' checks that the body of a response matches the expected
-- value.
assertBody s r = liftIO $ assertBool msg $ s == body
  where
    msg  = "Expected response body " ++ ss s ++ ", but received " ++ ss body
    ss   = show . L8.unpack
    body = simpleBody r

assertRes :: Int -> L.ByteString -> SResponse -> Session ()
-- | 'assertRes' checks that the status code and body of a response match the
-- expected values.
assertRes code string response =
  assertStatus code response >> assertBody string response

runSqliteSession :: Session () -> IO ()
-- | 'runSqliteSession' runs 'serve' with an empty in-memory database.
runSqliteSession s = withSqlitePool ":memory:" 1 runAppSession
  where runAppSession pool = runSession s =<< scottyApp (serve pool)

firstKey :: L.ByteString
-- | 'firstKey' is the text representation of the first inserted key in a
-- table.
firstKey = "Key {unKey = PersistInt64 1}"

firstTimespans :: L.ByteString
-- | 'firstTimespans' is the expected response body for a timespan query that
-- matches that inserted by 'initTimespan'.
firstTimespans =
  L.concat
    ["[(Entity {entityKey = "
    ,firstKey
    ,", entityVal = Timespan {timespanParent = Nothing, timespanClock = "
    ,firstKey
    ,", timespanBeginMin = -10.0, timespanBeginMax = -9.0, "
    ,"timespanEndMin = 9.0, timespanEndMax = 10.0, timespanWeight = 1.0}},"
    ,"[])]"]

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertRes 200 firstKey

initTimespan :: Session ()
-- | 'initTimespan' does 'initClock', then inserts a timespan and checks the
-- response.
initTimespan = initClock >> post "/timespans" body >>= assertRes 200 firstKey
  where body = "clock=TT&beginMin=-10.0&beginMax=-9.0&endMin=9.0&endMax=10.0"

getTimespans :: (Double, Double) -> Session SResponse
-- | 'getTimespans' performs a query for timespans in the given time range.
getTimespans (a, b) =
  get $ B.concat ["/timespans?clock=TT&begin=", ps a, "&end=", ps b]
  where ps = B8.pack . show

it :: String -> Session () -> Spec
-- | 'it' creates a 'Spec' item wrapped around 'runSqliteSession'.
it label action = HS.it label $ hSilence [stderr] $ runSqliteSession action

spec :: Spec
-- | 'spec' is the 'Spec' for the tempuhs web server application.
spec = do
  describe "POST /clocks" $ do
    it "inserts a clock with key 1"
      initClock
    it "won't insert two clocks with the same name" $ do
      initClock
      post  "/clocks" "name=TT" >>= assertStatus 500
  describe "POST /timespans" $
    it "inserts a timespan with key 1"
      initTimespan
  describe "GET /timespans" $ do
    it "initially returns [] for an existing clock" $ do
      initClock
      get "/timespans?clock=TT&begin=0&end=0" >>= assertRes 200 "[]"
    it "returns all timespans that touch or intersect the view" $ do
      initTimespan
      forM_ [(0, 0), (-11, -10), (10, 11), (-11, 11)] $
        getTimespans >=> assertRes 200 firstTimespans
    it "returns [] for views that don't intersect any timespan" $ do
      initTimespan
      forM_ [(-12, -11), (11, 12)] $ getTimespans >=> assertRes 200 "[]"

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
