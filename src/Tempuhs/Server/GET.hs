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
import Web.Scotty
  (
  ActionM,
  json,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  getAttrs,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Output
  (
  errInvalidParam,
  jsonError,
  )
import Tempuhs.Server.Param
  (
  maybeParam,
  )

timespans :: ConnectionPool -> ActionM ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans p = do
  parent <- maybeParam "parent"
  clock  <- maybeParam "clock"
  begin  <- maybeParam "begin"
  end    <- maybeParam "end"
  let filters = (TimespanParent ==. (mkKey <$> parent)) :
                  [TimespanBeginMin <=. x | x <- toList end] ++
                  [TimespanEndMax   >=. x | x <- toList begin]
  join $ runDatabase p $ do
    clockFilter <- case clock of
      Just i  -> do
        maybeClock <- getBy (UniqueClock i)
        return $ (\x -> [TimespanClock ==. entityKey x]) <$> maybeClock
      Nothing -> return $ Just []
    case clockFilter of
      Just cf -> do
        list <- selectList (cf ++ filters) []
        json <$> mapM (\e -> (,) e <$> getAttrs e) list
      Nothing -> return $ jsonError $ errInvalidParam "clock"
