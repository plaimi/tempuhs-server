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
  (<=.),
  (==.),
  (>=.),
  entityKey,
  getBy,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  json,
  raise,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  getAttrs,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  ParsableTime (fromParsableTime),
  maybeParam,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  errInvalidParam,
  )

timespans :: ConnectionPool -> ActionE ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans p = do
  parent  <- maybeParam "parent"
  clock   <- maybeParam "clock"
  begin   <- maybeParam "begin"
  end     <- maybeParam "end"
  rubbish <- maybeParam "rubbish"
  let filters = (TimespanParent ==. (mkKey <$> parent))                :
                (let (#) = case rubbish of
                             Just _  -> (>=.)
                             Nothing -> (==.)
                 in  TimespanRubbish # (fromParsableTime <$> rubbish)) :
                [TimespanBeginMin <=. x | x <- toList end]             ++
                [TimespanEndMax   >=. x | x <- toList begin]
  runDatabase p $ do
    clockFilter <- case clock of
      Just i  -> do
        maybeClock <- getBy (UniqueClock i)
        return $ (\x -> [TimespanClock ==. entityKey x]) <$> maybeClock
      Nothing -> return $ Just []
    case clockFilter of
      Just cf -> do
        list <- selectList (cf ++ filters) []
        liftAE . json =<< mapM (\e -> (,) e <$> getAttrs e) list
      Nothing -> liftAE $ raise $ errInvalidParam "clock"

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  name <- maybeParam "name"
  cid  <- maybeParam "id"
  let filters = [ClockName ==. x | x <- toList name] ++
                  [ClockId ==. mkKey x | x <- toList cid]
  runDatabase p $ liftAE . json =<< selectList filters []
