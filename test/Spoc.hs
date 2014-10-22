{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Utilities for the tempuhs web server application tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc where

import Control.Monad.Logger
  (
  NoLoggingT (NoLoggingT),
  runNoLoggingT,
  )
import Database.Persist.Sqlite
  (
  withSqlitePool,
  )
import Network.Wai.Test
  (
  Session,
  SResponse,
  runSession,
  )
import System.IO
  (
  stderr,
  )
import System.IO.Silently
  (
  hSilence,
  )
import Test.Hspec
  (
  Spec,
  )
import qualified Test.Hspec as HS
  (
  it,
  )

import Tempuhs.Server
  (
  serve,
  )
import Tempuhs.Server.Spock
  (
  scottyAppE,
  )
import Spoc.Assert
  (
  assertJSONError,
  )

runSqliteSession :: Session () -> IO ()
-- | 'runSqliteSession' runs 'serve' with an empty in-memory database.
runSqliteSession s =
  runNoLoggingT $ withSqlitePool ":memory:" 1 $
    \pool -> NoLoggingT $ runSession s =<< scottyAppE (serve pool)

it :: String -> Session () -> Spec
-- | 'it' creates a 'Spec' item wrapped around 'runSqliteSession'.
it label action = HS.it label $ hSilence [stderr] $ runSqliteSession action

itReturnsMissingParam :: Session SResponse -> Spec
-- | 'itReturnsMissingParam' is a convenience function that creates a 'Spec'
-- item for responses that should fail with "MISSING_PARAM".
itReturnsMissingParam s =
  it "fails with MISSING_PARAM when required parametres are missing" $
    s >>= assertJSONError 400 "MISSING_PARAM"
