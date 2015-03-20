{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  DELETE Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.DELETE (
  attributesRubbishSpec,
  rubbishSpec,
  unsafeRubbishSpec,
  ) where

import Data.Aeson
  (
  FromJSON,
  ToJSON,
  )
import Data.ByteString.Char8
  (
  pack,
  )
import Data.Time.Clock
  (
  UTCTime,
  )
import Network.Wai.Test
  (
  Session,
  SResponse,
  )
import Test.Hspec
  (
  Spec,
  describe,
  )

import Tempuhs.Spoc
  (
  it,
  )
import Tempuhs.Spoc.Assert
  (
  assertJSON,
  assertJSONError,
  assertJSONOK,
  assertStatus,
  )
import Tempuhs.Spoc.Entity
  (
  (=^=),
  (=^^=),
  )
import Tempuhs.Spoc.JSON
  (
  jsonSuccess,
  showJSON,
  )
import Tempuhs.Spoc.Request
  (
  delete,
  get,
  )

import Tempuhs.Chronology


du :: String -> String -> Session SResponse
du f s = delete (pack $ "/" ++ f ++ "s" ++ s ++ f ++ "=1")

ru :: String -> Session SResponse
ru = flip du "?"

pu :: String -> Session SResponse
pu = flip du "/purge?"

rubbishSpec :: (HasRubbish d (Maybe UTCTime), FromJSON d, ToJSON d)
            => String -> Session () -> [d] -> Spec
rubbishSpec f i d =
  describe ("DELETE /" ++ f ++ "s") $ do
    it ("rubbishes a " ++ f) $ i >> ru f >>= assertJSONOK jsonSuccess
    it ("returns the rubbished " ++ f) $ do
      i >> ru f >>= assertJSONOK jsonSuccess
      get (pack $ "/" ++ f ++ "s?rubbish=2000-01-01") >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON d) r (d =^=)

unsafeRubbishSpec :: [Char] -> Session a -> Spec
unsafeRubbishSpec f i =
  describe ("DELETE /" ++ f ++ "s/purge") $ do
    it "initially returns []" $ get (pack $ f ++ "s") >>= assertJSONOK ()
    it ("refuses to purge a non-rubbish " ++ f) $
      i >> pu f >>= assertJSONError 400 "INVALID_PARAM"
    it ("purges a " ++ f) $ i >> ru f >> pu f >>= assertJSONOK jsonSuccess
    it "returns []" $ get (pack $ f ++ "s") >>= assertJSONOK ()

attributesRubbishSpec :: (HasRubbish d (Maybe UTCTime)
                         ,HasRubbish a (Maybe UTCTime)
                         ,FromJSON d, ToJSON d
                         ,FromJSON a, ToJSON a)
                      => String -> Session () -> [(d, [a])] -> Spec
attributesRubbishSpec f i d =
  describe ("DELETE /" ++ f ++ "s") $ do
    it ("rubbishes a " ++ f) $ i >> ru f >>= assertJSONOK jsonSuccess
    it ("returns the rubbished " ++ f) $ do
      i >> ru f >>= assertJSONOK jsonSuccess
      get (pack $ "/" ++ f ++ "s?rubbish=2000-01-01") >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON d) r (d =^^=)
