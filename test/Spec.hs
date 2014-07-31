{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Spec for the tempuhs web server application
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main where

import Control.Monad
  (
  (>=>),
  forM_,
  )
import Control.Monad.IO.Class
  (
  liftIO,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text
  (
  Text,
  )
import Database.Persist
  (
  Entity (Entity),
  )
import Database.Persist.Sqlite
  (
  withSqlitePool,
  )
import Network.HTTP.Types
  (
  statusCode,
  )
import Network.Wai
  (
  Request,
  requestHeaders,
  requestMethod,
  )
import Network.Wai.Test
  (
  Session,
  SResponse,
  SRequest (SRequest),
  defaultRequest,
  request,
  runSession,
  setPath,
  simpleBody,
  simpleStatus,
  srequest,
  )
import System.IO
  (
  stderr,
  )
import System.IO.Silently
  (
  hSilence,
  )
import Test.HUnit
  (
  assertBool,
  )
import Test.Hspec
  (
  Spec,
  describe,
  hspec,
  )
import qualified Test.Hspec as HS
import Web.Scotty
  (
  scottyApp,
  )

import Plailude
import Tempuhs.Chronology
import Tempuhs.Server
  (
  serve,
  )
import Tempuhs.Server.Database
  (
  mkKey,
  )

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
    msg  = "Expected response body " ++ show s ++
           ", but received " ++ show body
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
firstKey = showL8 $ mkKey 1

firstTimespanEntity :: Entity Timespan
-- | 'firstTimespanEntity' is equal to the data inserted by 'initTimespan'.
firstTimespanEntity =
  Entity (mkKey 1) $ Timespan Nothing (mkKey 1) (-10) (-9) 9 10 1

firstTimespans :: [Entity TimespanAttribute] -> L.ByteString
-- | 'firstTimespans' is the expected response body for a timespan query that
-- matches that inserted by 'initTimespan', together with the given list of
-- attribute entities.
firstTimespans attrs = showL8 [(firstTimespanEntity, attrs)]

attributeEntity :: Integer -> Integer -> Text -> Text ->
                   Entity TimespanAttribute
-- | 'attributeEntity' is a convenience function for constructing
-- an 'Entity' containing a 'TimespanAttribute'.
attributeEntity k = Entity (mkKey k) .:. (TimespanAttribute . mkKey)

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertRes 200 firstKey

initTimespan :: Session ()
-- | 'initTimespan' does 'initClock', then inserts a timespan and checks the
-- response.
initTimespan = initClock >> post "/timespans" body >>= assertRes 200 firstKey
  where body = "clock=TT&beginMin=-10.0&beginMax=-9.0&endMin=9.0&endMax=10.0"

initSubTimespan :: Session ()
-- | 'initSubTimespan' does 'initTimespan', then inserts another timespan with
-- the first timespan as parent and checks the response.
initSubTimespan =
  initTimespan >> post "/timespans" body >>= assertRes 200 secondKey
  where
    body =
      "parent=1&clock=TT&beginMin=-9.0&beginMax=-8.0&endMin=8.0&endMax=9.0"
    secondKey = showL8 $ mkKey 2

initAttribute :: Session ()
-- | 'initAttribute' does 'initTimespan', then inserts a timespan attribute
-- and checks the response.
initAttribute =
  initTimespan >> post "/attributes" body >>= assertRes 200 firstKey
  where body = "timespan=1&key=title&value=test"

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
  describe "POST /timespans" $ do
    it "inserts a timespan with key 1"
      initTimespan
    it "successfully inserts a sub-timespan" $ do
      initSubTimespan
  describe "POST /attributes" $ do
    it "inserts a timespan attribute with key 1"
      initAttribute
    it "modifies an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title&value=new" >>=
        assertRes 200 firstKey
      getTimespans (0, 0) >>=
        assertRes 200 (firstTimespans [attributeEntity 1 1 "title" "new"])
    it "deletes an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title" >>= assertRes 200 ""
      getTimespans (0, 0) >>= assertRes 200 (firstTimespans [])
  describe "GET /timespans" $ do
    it "initially returns [] for an existing clock" $ do
      initClock
      get "/timespans?clock=TT&begin=0&end=0" >>= assertRes 200 "[]"
    it "returns all timespans that touch or intersect the view" $ do
      initTimespan
      forM_ [(0, 0), (-11, -10), (10, 11), (-11, 11)] $
        getTimespans >=> assertRes 200 (firstTimespans [])
    it "returns [] for views that don't intersect any timespan" $ do
      initTimespan
      forM_ [(-12, -11), (11, 12)] $ getTimespans >=> assertRes 200 "[]"
    it "returns associated timespan attributes" $ do
      initAttribute
      getTimespans (0, 0) >>=
        assertRes 200 (firstTimespans [attributeEntity 1 1 "title" "test"])

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
