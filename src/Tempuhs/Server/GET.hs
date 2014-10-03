{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server GET API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.GET where

import Data.Foldable
  (
  toList,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  SelectOpt (Asc),
  (<=.),
  (==.),
  (>=.),
  entityKey,
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

import Plailude
import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  clockParam,
  getAttrs,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  ParsableWrapper (parsableUnwrap),
  maybeParam,
  rescueMissing,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  )

timespans :: ConnectionPool -> ActionE ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans p = do
  parent  <- maybeParam "parent"
  begin   <- maybeParam "begin"
  end     <- maybeParam "end"
  rubbish <- maybeParam "rubbish"
  let filters = (TimespanParent ==. (mkKey <$> parent))              :
                (let (#) = case rubbish of
                             Just _  -> (>=.)
                             Nothing -> (==.)
                 in  TimespanRubbish # (parsableUnwrap <$> rubbish)) :
                [TimespanBeginMin <=. x | x <- toList end]           ++
                [TimespanEndMax   >=. x | x <- toList begin]
  runDatabase p $ do
    clock <- liftAE . rescueMissing =<< erretreat (clockParam "clock")
    let clockFilter = [TimespanClock ==. entityKey c | c <- toList clock]
    list <- selectList (clockFilter ++ filters) [Asc TimespanId]
    liftAE . json =<< mapM (\e -> (,) e <$> getAttrs e) list

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  name <- maybeParam "name"
  cid  <- maybeParam "id"
  let filters = [ClockName ==. x | x <- toList name] ++
                  [ClockId ==. mkKey x | x <- toList cid]
  runDatabase p $ liftAE . json =<< selectList filters [Asc ClockId]
