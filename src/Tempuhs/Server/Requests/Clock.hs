{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Clock API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Clock where

import Data.Foldable
  (
  toList,
  )
import Database.Persist
  (
  SelectOpt (Asc),
  (==.),
  insert,
  repsert,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  json,
  )

import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  maybeParam,
  paramE,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  )

postClock :: ConnectionPool -> ActionE ()
-- | 'postClock' inserts a new 'Clock' into the database, or updates an
-- existing one, from a request.
postClock p = do
  c <- maybeParam "clock"
  n <- paramE     "name"
  runDatabase p $
    let d = Clock n
    in  liftAE . jsonKey =<< case c of
      Just i  -> let k = mkKey i
                 in  repsert k d >> return k
      Nothing -> insert d

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [ClockName ==. cn       | cn <- toList n] ++
                [ClockId   ==. mkKey ci | ci <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc ClockId]
