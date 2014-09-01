{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server POST API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.POST where

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
import Web.Scotty.Trans
  (
  param,
  raise,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  defaultParam,
  maybeParam,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  errInvalidParam,
  jsonKey,
  jsonSuccess,
  )

postTimespan :: ConnectionPool -> ActionE ()
-- | 'postTimespan' inserts a new 'Timespan' into the database, or updates an
-- existing one, from a request.
postTimespan p = do
  timespan      <- maybeParam     "timespan"
  parent        <- maybeParam     "parent"
  clock         <- param          "clock"
  beginMin      <- param          "beginMin"
  maybeBeginMax <- maybeParam     "beginMax"
  maybeEndMin   <- maybeParam     "endMin"
  maybeEndMax   <- maybeParam     "endMax"
  weight        <- defaultParam 1 "weight"

  -- If beginMax isn't specified, set it to beginMin + 1
  let beginMax           = fromMaybe ((+( 1 :: ProperTime)) beginMin)
                                     maybeBeginMax
  -- If endMin isn't specified, set it to endMax-1.
  -- If endMax isn't specified, set it to endMin+1.
  -- If neither are specified, set them to beginMax and beginMax+1.
      (endMin, endMax)   =
        case (maybeEndMin, maybeEndMax) of
          (Nothing, Nothing) ->
            (beginMin, fromMaybe (beginMin + (1 :: ProperTime)) maybeBeginMax)
          (Just a, Nothing)  -> (a, a + (1 :: ProperTime))
          (Nothing, Just b)  -> (b + (-1 :: ProperTime), b)
          (Just a, Just b)   -> (a, b)

  runDatabase p $ do
    maybeClock <- getBy $ UniqueClock clock
    case maybeClock of
      Just (Entity clockKey _) ->
        let ts = Timespan (mkKey <$> parent) clockKey beginMin beginMax
                 endMin endMax weight
        in  liftAE . jsonKey =<< case timespan of
          Just i  -> let k = mkKey i
                     in  repsert k ts >> return k
          Nothing -> insert ts
      Nothing                  ->
        liftAE $ raise $ errInvalidParam "clock"

postAttribute :: ConnectionPool -> ActionE ()
-- | 'postAttribute' sets or removes a 'TimespanAttribute' based on a request.
postAttribute p = do
  timespan <- param      "timespan"
  key      <- param      "key"
  value    <- maybeParam "value"
  runDatabase p $
    let tsId = mkKey timespan
    in do
      maybeAttribute <- getBy $ UniqueTimespanAttribute tsId key
      case value of
        Just v  ->
          liftAE . jsonKey =<< case maybeAttribute of
            Just (Entity attrId _) ->
              update attrId [TimespanAttributeValue =. v] >> return attrId
            Nothing                ->
              insert $ TimespanAttribute tsId key v
        Nothing -> do
          case maybeAttribute of
            Just (Entity attrId _) -> delete attrId
            Nothing                -> return ()
          liftAE jsonSuccess

postClock :: ConnectionPool -> ActionE ()
-- | 'postClock' inserts a new 'Clock' into the database, or updates an
-- existing one, from a request.
postClock p = do
  clock <- maybeParam "clock"
  name  <- param "name"
  runDatabase p $
    let c = Clock name
    in  liftAE . jsonKey =<< case clock of
      Just i  -> let k = mkKey i
                 in  repsert k c >> return k
      Nothing -> insert c
