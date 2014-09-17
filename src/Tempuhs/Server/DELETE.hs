{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server DELETE API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.DELETE where

import Control.Monad.IO.Class
  (
  liftIO,
  )
import Data.Time.Clock
  (
  getCurrentTime,
  )
import Database.Persist
  (
  Key,
  (=.),
  get,
  update,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  param,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  errInvalidParam,
  jsonError,
  jsonSuccess,
  )

deleteTimespan :: ConnectionPool -> ActionE ()
-- | 'deleteTimespan' updates the rubbish field of an existing 'Timespan'.
deleteTimespan p = do
  timespan <- param  "timespan"
  now      <- liftIO getCurrentTime
  runDatabase p $ do
    let tsId = mkKey timespan :: Key Timespan
    maybeTimespan <- get tsId
    case maybeTimespan of
      Just _  ->
        update tsId [TimespanRubbish =. Just now] >> liftAE jsonSuccess
      Nothing -> liftAE $ jsonError $ errInvalidParam "timespan"
