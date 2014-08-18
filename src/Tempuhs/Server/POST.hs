{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server POST API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.POST where

import Control.Monad
  (
  join,
  )
import Data.Maybe
  (
  fromMaybe,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  Entity (Entity),
  (=.),
  getBy,
  delete,
  insert,
  repsert,
  update,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty
  (
  ActionM,
  param,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Output
  (
  errInvalidParam,
  jsonError,
  jsonKey,
  jsonSuccess,
  )
import Tempuhs.Server.Param
  (
  defaultParam,
  maybeParam,
  )

postTimespan :: ConnectionPool -> ActionM ()
-- | 'postTimespan' inserts a new 'Timespan' into the database, or updates an
-- existing one, from a request.
postTimespan p = do
  timespan      <- maybeParam     "timespan"
  parent        <- maybeParam     "parent"
  clock         <- param          "clock"
  beginMin      <- param          "beginMin"
  maybeBeginMax <- maybeParam     "beginMax"
  maybeEndMin   <- maybeParam     "endMin"
  endMax        <- param          "endMax"
  weight        <- defaultParam 1 "weight"

  -- If beginMax/endMin are not specified, set them to beginMin+1/endMax-1.
  let beginMax = fromMaybe ((+( 1 :: ProperTime)) beginMin) maybeBeginMax
      endMin   = fromMaybe ((+(-1 :: ProperTime)) endMax)   maybeEndMin

  join $ runDatabase p $ do
    maybeClock <- getBy $ UniqueClock clock
    case maybeClock of
      Just (Entity clockKey _) ->
        let ts = Timespan (mkKey <$> parent) clockKey beginMin beginMax
                 endMin endMax weight
        in  return . jsonKey =<< case timespan of
          Just i  -> let k = mkKey i
                     in  repsert k ts >> return k
          Nothing -> insert ts
      Nothing                  ->
        return $ jsonError $ errInvalidParam "clock"

postAttribute :: ConnectionPool -> ActionM ()
-- | 'postAttribute' sets or removes a 'TimespanAttribute' based on a request.
postAttribute p = do
  timespan <- param      "timespan"
  key      <- param      "key"
  value    <- maybeParam "value"
  join $ runDatabase p $
    let tsId = mkKey timespan
    in do
      maybeAttribute <- getBy $ UniqueTimespanAttribute tsId key
      case value of
        Just v  ->
          return . jsonKey =<< case maybeAttribute of
            Just (Entity attrId _) ->
              update attrId [TimespanAttributeValue =. v] >> return attrId
            Nothing                ->
              insert $ TimespanAttribute tsId key v
        Nothing -> do
          case maybeAttribute of
            Just (Entity attrId _) -> delete attrId
            Nothing                -> return ()
          return jsonSuccess

postClock :: ConnectionPool -> ActionM ()
-- | 'postClock' inserts a new 'Clock' into the database from a request.
postClock p = do
  name <- param "name"
  join $ runDatabase p $ do
    k <- insert $ Clock name
    return $ jsonKey k
