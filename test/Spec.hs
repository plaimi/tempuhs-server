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
  guard,
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
import Data.List
  (
  intercalate,
  subsequences,
  )
import Data.Maybe
  (
  fromMaybe,
  isJust,
  )
import qualified Data.Set as Z
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
  methodDelete,
  methodPost,
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
import Tempuhs.Server.Spock
  (
  scottyAppE,
  )


specifiedsSet :: Z.Set String
-- | A 'Z.Set' of all optional values that are specified.
specifiedsSet = Z.fromList ["beginMax", "endMin", "endMax"]

formPostRequest :: Request
-- | 'formPostRequest' is a blank POST request for use with URL-encoded form
-- data as the body.
formPostRequest = defaultRequest
  { requestMethod = methodPost
  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]}

get :: B.ByteString -> Session SResponse
-- | 'get' makes a GET 'request' with the given path.
get = request . setPath defaultRequest

post :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'post' makes a POST 'request' with the given path and URL-encoded form
-- data as the body.
post p b = srequest $ SRequest (setPath formPostRequest p) b

delete :: B.ByteString -> Session SResponse
-- | 'delete' makes a DELETE 'request' with the given path.
delete = request . setPath defaultRequest { requestMethod = methodDelete }

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
    \pool -> runSession s =<< scottyAppE (serve pool)

jsonKey :: Integer -> Value
-- | 'jsonKey' is the json representation of a database key.
jsonKey k = object ["id" .= mkKey k]

jsonSuccess :: Value
-- | 'jsonSuccess' is the result of a successful operation without any data to
-- return.
jsonSuccess = object []

clockEntity :: Integer -> Text -> Entity Clock
-- | 'clockEntity' is a convenience function for constructing an 'Entity'
-- containing a 'Clock'.
clockEntity k = Entity (mkKey k) . Clock

rubbishP :: [(Entity Timespan, [Entity TimespanAttribute])] ->
            [(Entity Timespan, [Entity TimespanAttribute])] ->
            Bool
-- | 'rubbishP' takes two get /timespan-results, and checks if the 'Timespan'
-- within the second result is the same as a rubbished version of the useful
-- timespan in the first result.
rubbishP ((Entity ek ev,_):es) ((f@(Entity _ fv),_):fs) =
  let fr = timespanRubbish fv
  in  isJust fr                                                &&
      toJSON (Entity ek ev {timespanRubbish = fr}) == toJSON f &&
      rubbishP es fs
rubbishP [] []                                          = True
rubbishP _  []                                          = False
rubbishP []  _                                          = False

firstTimespanEntity :: Z.Set String -> Entity Timespan
-- | 'firstTimespanEntity' is equal to the data inserted by 'initTimespan'
-- with the appropriate behaviour depending on which optionals are specified
-- in the 'Z.Set' of specifieds.
firstTimespanEntity ss =
  Entity (mkKey 1) $
    Timespan Nothing (mkKey 1) 10 beginMax endMin endMax 1 Nothing
  where
    (beginMax, endMin, endMax) =
      case ("beginMax" `Z.member` ss
           ,"endMin"   `Z.member` ss
           ,"endMax"   `Z.member` ss) of
        (False, False, False) -> (11, 10, 11)
        (True,  False, False) -> (15, 10, 15)
        (True,  True,  False) -> (15, 24, 25)
        (False, True,  False) -> (11, 24, 25)
        (True,  False, True)  -> (15, 41, 42)
        (False, True,  True)  -> (11, 24, 42)
        (False, False, True)  -> (11, 41, 42)
        (True,  True,  True)  -> (15, 24, 42)

subTimespanEntity :: Entity Timespan
-- | 'subTimespanEntity' is equal to the data inserted by 'initSubTimespan'.
subTimespanEntity =
  Entity (mkKey 2) $
    Timespan (Just $ mkKey 1) (mkKey 1) (-9) (-8) 8 9 1 Nothing

modTimespanEntity :: Entity Timespan
-- | 'modTimespanEntity' is equal to the data inserted by 'initModTimespan'.
modTimespanEntity =
  Entity (mkKey 1) $ Timespan Nothing (mkKey 1) 0 1 9 10 1 Nothing

defaultTimespans :: [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'defaultTimespans' is a helper value for the often used 'firstTimespans'
-- with 'specifedsSet' and '[]'.
defaultTimespans = firstTimespans specifiedsSet []

firstTimespans :: Z.Set String -> [Entity TimespanAttribute] ->
                  [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'firstTimespans' is the expected response for a timespan query that
-- matches what 'initTimespan' inserted, depending on which fields are to be
-- left unspecified per the 'Z.Set' of specifieds, and the given
-- '[Entity TimespanAttribute]'.
firstTimespans ss attrs = [(firstTimespanEntity ss, attrs)]

attributeEntity :: Integer -> Integer -> Text -> Text ->
                   Entity TimespanAttribute
-- | 'attributeEntity' is a convenience function for constructing
-- an 'Entity' containing a 'TimespanAttribute'.
attributeEntity k = Entity (mkKey k) .:. (TimespanAttribute . mkKey)

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 1)

initTimespan :: Z.Set String -> Session ()
-- | 'initTimeSpan' does 'initClock', then inserts a timespan specifying the
-- optionals that are members of the 'Z.Set' of specifieds (falling back to
-- default values for optionals not in the set), and checks the response.
initTimespan ss =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = L8.pack . concat $
          ["clock=TT&beginMin=10.0"]                  ++
          ["&beginMax=15" | "beginMax" `Z.member` ss] ++
          ["&endMin=24"   | "endMin"   `Z.member` ss] ++
          ["&endMax=42"   | "endMax"   `Z.member` ss]

initSubTimespan :: Session ()
-- | 'initSubTimespan' does 'initTimespan', then inserts another timespan with
-- the first timespan as parent and checks the response.
initSubTimespan =
  initTimespan specifiedsSet >>
    post "/timespans" body >>= assertJSONOK (jsonKey 2)
  where
    body =
      "parent=1&clock=TT&beginMin=-9.0&beginMax=-8.0&endMin=8.0&endMax=9.0"

initModTimespan :: Session ()
-- | 'initModTimespan' does 'initTimespan', then modifies the existing
-- timespan and checks the response.
initModTimespan =
  initTimespan specifiedsSet >>
    post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = "timespan=1&clock=TT&beginMin=0&beginMax=1&endMin=9&endMax=10"

initAttribute :: Session ()
-- | 'initAttribute' does 'initTimespan', then inserts a timespan attribute
-- and checks the response.
initAttribute =
  initTimespan specifiedsSet >>
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
    it "modifies an existing clock" $ do
      initClock
      post "/clocks" "clock=1&name=TT2" >>= assertJSONOK (jsonKey 1)
      post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 2)
  describe "POST /timespans" $ do
    forM_ (subsequences . Z.toList $ specifiedsSet) $
      \ss -> it ("inserts a timespan with key 1 specifying " ++
                  intercalate "/" ss) $ do
        initTimespan $ Z.fromList ss
        getTimespans (10, 42) >>=
          assertJSONOK (firstTimespans (Z.fromList ss) [])
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
        assertJSONOK (firstTimespans specifiedsSet
                                     [attributeEntity 1 1 "title" "new"])
    it "deletes an existing timespan attribute" $ do
      initAttribute
      post "/attributes" "timespan=1&key=title" >>= assertJSONOK jsonSuccess
      getTimespans (10, 42) >>= assertJSONOK defaultTimespans
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
  describe "GET /timespans" $ do
    it "initially returns []" $
      get "/timespans" >>= assertJSONOK ()
    it "initially returns [] for an existing clock" $ do
      initClock
      get "/timespans?clock=TT&begin=0&end=0" >>= assertJSONOK ()
    it "returns all timespans that touch or intersect the view" $ do
      initTimespan specifiedsSet
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
      initTimespan specifiedsSet
      forM_ [(0, 9), (43, 50)] $ getTimespans >=> assertJSONOK ()
      forM_ ["begin=43", "end=9"] $
        get . B.append "/timespans?" >=> assertJSONOK ()
    it "returns associated timespan attributes" $ do
      initAttribute
      getTimespans (10, 42) >>=
        assertJSONOK (firstTimespans specifiedsSet
                                     [attributeEntity 1 1 "title" "test"])
    it "filters on parent" $ do
      initSubTimespan
      get "/timespans" >>= assertJSONOK defaultTimespans
      get "/timespans?parent=1" >>=
        assertJSONOK [(subTimespanEntity, [] :: [()])]
    it "returns [] asking for rubbish after inserting useful timespans" $ do
      initTimespan specifiedsSet
      get "/timespans?rubbish=2000-01-01" >>= assertJSONOK ()
  describe "DELETE /timespans" $ do
    it "rubbishes a timespan" $ do
      initTimespan specifiedsSet
      delete "/timespans?timespan=1" >>= assertJSONOK jsonSuccess
    it "returns the rubbished timespan" $ do
      initTimespan specifiedsSet
      delete "/timespans?timespan=1" >>= assertJSONOK jsonSuccess
      get "/timespans?rubbish=2000-01-01" >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON defaultTimespans)
                   r (rubbishP defaultTimespans)

main :: IO ()
-- | 'main' runs 'spec' using 'hspec'.
main = hspec spec
