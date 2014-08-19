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
  mzero,
  )
import Control.Monad.IO.Class
  (
  liftIO,
  )
import Data.Aeson
  (
  FromJSON,
  ToJSON,
  Value (Object),
  (.=),
  encode,
  decode,
  object,
  toJSON,
  )
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  fromMaybe,
  )
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
  simpleHeaders,
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
import qualified Test.HUnit as HU
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

-- | 'Optionals' dictate what optional parameter for a POST is going to be
-- left unspecified.
data Optionals = MkOptionals
  {optionalBeginMax  :: Bool -- ^ Whether beginMax is optional or not.
  ,optionalEndMin    :: Bool -- ^ Whether endMin is optional or not.
  ,optionalEndMax    :: Bool -- ^ Whether endMax is optional or not.
  }

noOptionals :: Optionals
-- | Helper value for when to not leave any parametre optional.
noOptionals = MkOptionals False False False

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
-- | 'post' makes a POST 'request' with the given path and URL-encoded form
-- data as the body.
post p b = srequest $ SRequest (setPath formPostRequest p) b

showJSON :: ToJSON a => a -> String
-- | 'showJSON' returns the JSON representation of the given value as a
-- 'String'.
showJSON = L8.unpack . encode

assertBool :: String -> Bool -> Session ()
-- | 'assertBool' lifts 'HU.assertBool' from the 'IO' monad.
assertBool = liftIO .: HU.assertBool

assertStatus :: Int -> SResponse -> Session ()
-- | 'assertStatus' checks that the status code of a response matches the
-- expected value.
assertStatus code r = assertBool msg $ code == status
  where
    msg     = "Expected status code:\t" ++ show code ++
              "\nbut received:\t\t"     ++ show status
    status  = statusCode $ simpleStatus r

assertContentType :: B.ByteString -> SResponse -> Session ()
-- | 'assertContentType' checks that the Content-Type header of a response
-- matches the expected value.
assertContentType ct r = assertBool msg $ Just ct == h
  where
    msg = "Expected content type:\t" ++ show ct ++
          "\nbut received:\t\t"      ++ fromMaybe "nothing" (show <$> h)
    h   = lookup "content-type" $ simpleHeaders r

assertBody :: L.ByteString -> SResponse -> Session ()
-- | 'assertBody' checks that the body of a response matches the expected
-- value.
assertBody s r = assertBool msg $ s == body
  where
    msg  = "Expected response body:\t" ++ show s ++
           "\nbut received:\t\t"       ++ show body
    body = simpleBody r

assertRes :: Int -> L.ByteString -> SResponse -> Session ()
-- | 'assertRes' checks that the status code and body of a response match the
-- expected values.
assertRes code string response =
  assertStatus code response >> assertBody string response

assertJSON :: (FromJSON a, ToJSON a) =>
              String -> SResponse -> (a -> Bool) -> Session ()
-- | 'assertJSON' checks that a response is a valid JSON response and checks
-- the data using the provided function.
assertJSON desc r f = do
  assertContentType "application/json" r
  assertBool msg $ (f <$> db) == Just True
  where
    msg  = "Expected response body:\t" ++ desc ++
           "\nbut received:\t\t" ++ fromMaybe ("invalid JSON " ++ show body)
                                              (showJSON <$> db)
    db   = decode body
    body = simpleBody r

assertJSONOK :: ToJSON a => a -> SResponse -> Session ()
-- | 'assertJSONOK' checks that a response has status "200 OK" and that it
-- returns the expected JSON data.
assertJSONOK v r =
  assertStatus 200 r >> assertJSON (showJSON v) r (toJSON v ==)

assertJSONError :: Int -> Text -> SResponse -> Session ()
-- | 'assertJSONError' checks that a response is a JSON-encoded error message
-- matching the expected status and error codes.
assertJSONError s c r = do
  assertStatus s r
  assertJSON ("containing " ++ showJSON obj) r $ \db -> ec db == Just c
  where
    ec  = AT.parseMaybe $
      \v -> case v of
              Object o -> o AT..: "error" >>= (AT..: "code")
              _        -> mzero
    obj = object ["error" .= object ["code" .= c]]

runSqliteSession :: Session () -> IO ()
-- | 'runSqliteSession' runs 'serve' with an empty in-memory database.
runSqliteSession s =
  withSqlitePool ":memory:" 1 $
    \pool -> runSession s =<< scottyApp (serve pool)

jsonKey :: Integer -> Value
-- | 'jsonKey' is the json representation of a database key.
jsonKey k = object ["id" .= mkKey k]

jsonSuccess :: Value
-- | 'jsonSuccess' is the result of a successful operation without any data to
-- return.
jsonSuccess = object []

firstTimespanEntity :: Optionals -> Entity Timespan
-- | 'firstTimespanEntity' is equal to the data inserted by 'initTimespan'with
-- the appropriate behaviour depending on the optionals given.
firstTimespanEntity os =
  Entity (mkKey 1) $ Timespan Nothing (mkKey 1) 10 begMax endMin endMax 1
  where
    (begMax, endMin, endMax) =
      case (optionalBeginMax os, optionalEndMin os, optionalEndMax os) of
        (True,  True,  True)  -> (11, 10, 11)
        (False, True,  True)  -> (15, 10, 15)
        (False, False, True)  -> (15, 24, 25)
        (True,  False, True)  -> (11, 24, 25)
        (False, True,  False) -> (15, 41, 42)
        (True,  False, False) -> (11, 24, 42)
        (True,  True,  False) -> (11, 41, 42)
        (False, False, False) -> (15, 24, 42)

modTimespanEntity :: Entity Timespan
-- | 'modTimespanEntity' is equal to the data inserted by 'initModTimespan'.
modTimespanEntity =
  Entity (mkKey 1) $ Timespan Nothing (mkKey 1) 0 1 9 10 1

firstTimespans :: Optionals -> [Entity TimespanAttribute] ->
                  [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'firstTimespans' is the expected response for a timespan query that
-- matches what 'initTimespan' inserted, depending on the 'Optionals' given
-- and the given '[Entity TimespanAttribute]'.
firstTimespans os attrs = [(firstTimespanEntity os, attrs)]

attributeEntity :: Integer -> Integer -> Text -> Text ->
                   Entity TimespanAttribute
-- | 'attributeEntity' is a convenience function for constructing
-- an 'Entity' containing a 'TimespanAttribute'.
attributeEntity k = Entity (mkKey k) .:. (TimespanAttribute . mkKey)

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 1)

initTimespan :: Optionals -> Session ()
-- | 'initTimeSpan' does 'initClock', then inserts a timespan without
-- specifying the optional values (falling back to default values) that are
-- set to 'True' in the given 'Optionals', and checks the response.
initTimespan os =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = L8.pack . concat $
          ["clock=TT&beginMin=10.0"]                     ++
          ["&beginMax=15" | not . optionalBeginMax $ os] ++
          ["&endMin=24"   | not . optionalEndMin   $ os] ++
          ["&endMax=42"   | not . optionalEndMax   $ os]

initSubTimespan :: Session ()
-- | 'initSubTimespan' does 'initTimespan', then inserts another timespan with
-- the first timespan as parent and checks the response.
initSubTimespan =
  initTimespan noOptionals >>
    post "/timespans" body >>= assertJSONOK (jsonKey 2)
  where
    body =
      "parent=1&clock=TT&beginMin=-9.0&beginMax=-8.0&endMin=8.0&endMax=9.0"

initModTimespan :: Session ()
-- | 'initModTimespan' does 'initTimespan', then modifies the existing
-- timespan and checks the response.
initModTimespan =
  initTimespan noOptionals >>
    post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = "timespan=1&clock=TT&beginMin=0&beginMax=1&endMin=9&endMax=10"

initAttribute :: Session ()
-- | 'initAttribute' does 'initTimespan', then inserts a timespan attribute
-- and checks the response.
initAttribute =
  initTimespan noOptionals >>
    post "/attributes" body >>= assertJSONOK (jsonKey 1)
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
      post  "/clocks" "name=TT" >>= assertJSONError 500 "INTERNAL"
  describe "POST /timespans" $ do
    it "inserts a timespan with key 1 (w/o specifying any optionals)" $ do
      let os = MkOptionals { optionalBeginMax = True
                           , optionalEndMin   = True
                           , optionalEndMax   = True }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying endMin/Max)" $ do
      let os = MkOptionals { optionalBeginMax = False
                           , optionalEndMin   = True
                           , optionalEndMax   = True }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying ekndMax)" $ do
      let os = MkOptionals { optionalBeginMax = False
                           , optionalEndMin   = False
                           , optionalEndMax   = True }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying beginMax/endMax)" $ do
      let os = MkOptionals { optionalBeginMax = True
                           , optionalEndMin   = False
                           , optionalEndMax   = True }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying endMin)" $ do
      let os = MkOptionals { optionalBeginMax = False
                           , optionalEndMin   = True
                           , optionalEndMax   = False }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying beginMax)" $ do
      let os = MkOptionals { optionalBeginMax = True
                           , optionalEndMin   = False
                           , optionalEndMax   = False }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (w/o specifying beginMax/endMin)" $ do
      let os = MkOptionals { optionalBeginMax = True
                           , optionalEndMin   = True
                           , optionalEndMax   = False }
      initTimespan os
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans os [])
    it "inserts a timespan with key 1 (specifying the optionals)" $ do
      initTimespan noOptionals
    it "successfully inserts a sub-timespan"
      initSubTimespan
    it "modifies an existing timespan" $ do
      initModTimespan
      getTimespans (10, 42) >>=
        assertJSONOK [(modTimespanEntity, [] :: [()])]
  describe "POST /attributes" $ do
    it "inserts a timespan attribute with key 1"
      initAttribute
    it "modifies an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title&value=new" >>=
        assertJSONOK (jsonKey 1)
      getTimespans (10, 42) >>=
        assertJSONOK (firstTimespans noOptionals
                                     [attributeEntity 1 1 "title" "new"])
    it "deletes an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title" >>= assertJSONOK jsonSuccess
      getTimespans (10, 42) >>= assertJSONOK (firstTimespans noOptionals [])
  describe "GET /timespans" $ do
    it "initially returns [] for an existing clock" $ do
      initClock
      get "/timespans?clock=TT&begin=0&end=0" >>= assertJSONOK ()
    it "returns all timespans that touch or intersect the view" $ do
      initTimespan noOptionals
      forM_ [(10, 42), (9, 41), (11, 43), (9, 43)] $
        getTimespans >=> assertJSONOK (firstTimespans noOptionals [])
    it "returns [] for views that don't intersect any timespan" $ do
      initTimespan noOptionals
      forM_ [(0, 9), (43, 50)] $ getTimespans >=> assertJSONOK ()
    it "returns associated timespan attributes" $ do
      initAttribute
      getTimespans (10, 42) >>=
        assertJSONOK (firstTimespans noOptionals
                                     [attributeEntity 1 1 "title" "test"])

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
