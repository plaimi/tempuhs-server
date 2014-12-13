{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Clock API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Clock where

import Control.Monad
  (
  void,
  )
import Data.Foldable
  (
  toList,
  )
import Database.Persist
  (
  Key,
  SelectOpt (Asc),
  (=.),
  (==.),
  insert,
  replace,
  update,
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
import Tempuhs.Server.DELETE
  (
  nowow,
  owow,
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

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [ClockName ==. cn       | cn <- toList n] ++
                [ClockId   ==. mkKey ci | ci <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc ClockId]

postClock :: ConnectionPool -> ActionE ()
-- | 'postClock' inserts a 'Clock'.
postClock p = do
  n <- paramE "name"
  runDatabase p $ liftAE . jsonKey =<< insert (Clock n Nothing)

replaceClock :: ConnectionPool -> ActionE ()
-- | 'replaceClock' replaces a 'Clock'.
replaceClock p = do
  c <- paramE "clock"
  n <- paramE "name"
  runDatabase p $ let k = mkKey c in liftAE . jsonKey =<< do
    replace k (Clock n Nothing)
    return k

deleteClock :: ConnectionPool -> ActionE ()
-- | 'deleteClock' updates the rubbish field of an existing 'Clock'.
deleteClock = nowow "clock" ClockRubbish

unsafeDeleteClock :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteClock' hard-deletes a 'Clock' from the database.
unsafeDeleteClock p = void $ (owow "clock" p :: ActionE (Maybe (Key Clock)))

patchClock :: ConnectionPool -> ActionE ()
-- | 'patchClock' modifies a 'Clock'.
patchClock p = do
  c <- paramE "clock"
  n <- paramE "name"
  void $ runDatabase p $ let k = mkKey c in liftAE . jsonKey =<< do
    update k [ClockName =. n]
    return k
