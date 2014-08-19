{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server GET API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.GET where

import Control.Monad
  (
  join,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  Entity (Entity),
  (<=.),
  (==.),
  (>=.),
  getBy,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty
  (
  ActionM,
  json,
  param,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  getAttrs,
  runDatabase,
  )
import Tempuhs.Server.Output
  (
  errInvalidParam,
  jsonError,
  )

timespans :: ConnectionPool -> ActionM ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans p = do
  clock <- param "clock"
  begin <- param "begin"
  end   <- param "end"
  join $ runDatabase p $ do
    maybeClock <- getBy $ UniqueClock clock
    case maybeClock of
      Just (Entity clockKey _) -> do
        list <- selectList [TimespanClock ==. clockKey
                           ,TimespanBeginMin <=. end
                           ,TimespanEndMax >=. begin] []
        json <$> mapM (\e -> (,) e <$> getAttrs e) list
      Nothing                  ->
        return $ jsonError $ errInvalidParam "clock"
