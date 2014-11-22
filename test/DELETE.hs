{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  DELETE Specs for the tempuhs web server application.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module DELETE (
  deleteSpec,
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

import Spoc
  (
  it,
  )
import Spoc.Assert
  (
  assertJSON,
  assertJSONOK,
  assertStatus,
  )
import Spoc.Entity
  (
  (=^=),
  defaultRole,
  defaultTimespans,
  defaultUser,
  )
import Spoc.Init
  (
  initDefaultTimespan,
  initRole,
  initUser,
  )
import Spoc.JSON
  (
  jsonSuccess,
  showJSON,
  )
import Spoc.Request
  (
  delete,
  get,
  )

import Tempuhs.Chronology

deleteSpec :: Spec
-- | 'deleteSpec' runs the DELETE 'Spec's.
deleteSpec = do
  timespansSpec
  rolesSpec
  usersSpec

rubbishSpec :: (HasRubbish d (Maybe UTCTime), FromJSON d, ToJSON d)
            => String -> Session () -> [d] -> Spec
rubbishSpec f i d = do
  describe ("DELETE /" ++ f ++ "s") $ do
    it ("rubbishes a " ++ f) initDelete
    it ("returns the rubbished " ++ f) $ do
      initDelete
      get (pack $ "/" ++ f ++ "s?rubbish=2000-01-01") >>= \r -> do
        assertStatus 200 r
        assertJSON ("a rubbished version of: " ++ showJSON d) r ((=^=) d)
  where
    initDelete = i >> delete (pack $ "/" ++ f ++ "s?" ++ f ++ "=1")
                   >>= assertJSONOK jsonSuccess


timespansSpec :: Spec
timespansSpec = rubbishSpec "timespan" initDefaultTimespan defaultTimespans

rolesSpec :: Spec
rolesSpec = rubbishSpec "role" initRole [defaultRole]

usersSpec :: Spec
usersSpec =  rubbishSpec "user" initUser [defaultUser]
