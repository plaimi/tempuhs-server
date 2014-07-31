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
  liftM,
  )
import Data.Text.Lazy
  (
  pack,
  )
import Database.Persist
  (
  Entity (Entity),
  (=.),
  getBy,
  delete,
  insert,
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
  text,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  defaultParam,
  maybeParam,
  )

postTimespan :: ConnectionPool -> ActionM ()
-- | 'postTimespan' inserts a new 'Timespan' into the database from a request.
postTimespan p = do
  parent   <- maybeParam "parent"
  clock    <- param      "clock"
  beginMin <- param      "beginMin"
  beginMax <- param      "beginMax"
  endMin   <- param      "endMin"
  endMax   <- param      "endMax"
  weight   <- defaultParam 1 "weight"
  join $ runDatabase p $ do
    maybeClock <- getBy $ UniqueClock clock
    case maybeClock of
      Just (Entity clockKey _) -> do
        k <- insert $ Timespan (liftM mkKey parent) clockKey
                      beginMin beginMax endMin endMax weight
        return $ text . pack . show $ k
      Nothing                  -> return $ text ""

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
          return . text . pack . show =<< case maybeAttribute of
            Just (Entity attrId _) ->
              update attrId [TimespanAttributeValue =. v] >> return attrId
            Nothing                ->
              insert $ TimespanAttribute tsId key v
        Nothing -> do
          case maybeAttribute of
            Just (Entity attrId _) -> delete attrId
            Nothing                -> return ()
          return $ text ""

postClock :: ConnectionPool -> ActionM ()
-- | 'postClock' inserts a new 'Clock' into the database from a request.
postClock p = do
  name <- param "name"
  join $ runDatabase p $ do
    k <- insert $ Clock name
    return $ text . pack . show $ k
