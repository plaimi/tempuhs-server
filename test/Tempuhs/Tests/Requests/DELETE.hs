{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  DELETE Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Tests.Requests.DELETE (
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
  assertJSONOK,
  assertStatus,
  )
import Tempuhs.Spoc.Entity
  (
  (=^=),
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

rubbishSpec :: (HasRubbish d (Maybe UTCTime), FromJSON d, ToJSON d)
            => String -> Session () -> [d] -> Spec
rubbishSpec f i d =
  describe ("DELETE /" ++ f ++ "s") $ do
    it ("rubbishes a " ++ f) initDelete
    it ("returns the rubbished " ++ f) $ do
      initDelete
      get (pack $ "/" ++ f ++ "s?rubbish=2000-01-01") >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON d) r (d =^=)
  where
    initDelete = i >> delete (pack $ "/" ++ f ++ "s?" ++ f ++ "=1")
                   >>= assertJSONOK jsonSuccess

unsafeRubbishSpec :: String -> Session () -> Spec
unsafeRubbishSpec f i =
  describe ("DELETE /" ++ f ++ "s/purge") $ do
    it ("purges a " ++ f) initDelete
    it "returns []" $ get (pack $ f ++ "s") >>= assertJSONOK ()
  where
    initDelete = i >>
                 delete (pack $ "/" ++ f ++ "s/purge?"
                                    ++ f ++ "=1") >>= assertJSONOK jsonSuccess
