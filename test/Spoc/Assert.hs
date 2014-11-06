{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Asserts for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Assert where

import Control.Monad
  (
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
  decode,
  object,
  toJSON,
  )
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  fromMaybe,
  )
import qualified Data.Text as T
import Network.HTTP.Types
  (
  statusCode,
  )
import Network.Wai.Test
  (
  Session,
  SResponse,
  simpleBody,
  simpleHeaders,
  simpleStatus,
  )
import qualified Test.HUnit as HU (assertBool)

import Plailude
import Spoc.JSON
  (
  showJSON,
  )

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
    h   = B8.takeWhile (/= ';') <$> lookup "content-type" (simpleHeaders r)

assertJSON :: (FromJSON a, ToJSON a)
           => String -> SResponse -> (a -> Bool) -> Session ()
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

assertJSONError :: Int -> T.Text -> SResponse -> Session ()
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

assertJSONOK :: ToJSON a => a -> SResponse -> Session ()
-- | 'assertJSONOK' checks that a response has status "200 OK" and that it
-- returns the expected JSON data.
assertJSONOK v r =
  assertStatus 200 r >> assertJSON (showJSON v) r (toJSON v ==)
