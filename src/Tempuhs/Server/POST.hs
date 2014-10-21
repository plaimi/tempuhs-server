{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server POST API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.POST where

import Control.Arrow
  (
  (***),
  first,
  )
import Control.Monad
  (
  forM_,
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
  entityKey,
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
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
  (
  params,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  clockParam,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  paramE,
  defaultParam,
  maybeParam,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  jsonSuccess,
  )

postTimespan :: ConnectionPool -> ActionE ()
-- | 'postTimespan' inserts a new 'Timespan' into the database with the given
-- attributes. Or, if the ID of an existing 'Timespan' is given, updates that
-- instead.
postTimespan p = do
  timespan      <- maybeParam     "timespan"
  parent        <- maybeParam     "parent"
  beginMin      <- paramE         "beginMin"
  maybeBeginMax <- maybeParam     "beginMax"
  maybeEndMin   <- maybeParam     "endMin"
  maybeEndMax   <- maybeParam     "endMax"
  weight        <- defaultParam 1 "weight"
  rubbish       <- return Nothing
  -- If it ends with an '_', consider it an attribute. "&foo_=fu&bar_=baz".
  -- attrs is a list of key-value tuples.
  attrs         <- filter (L.isSuffixOf "_" . fst) <$> params

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
    clock <- clockParam "clock"
    let ts = Timespan (mkKey <$> parent) (entityKey clock) beginMin beginMax
             endMin endMax weight rubbish
    liftAE . jsonKey =<< case timespan of
      Just i  -> let k = mkKey i
                 in  repsert k ts >> return k
      Nothing -> do
        tid <- insert ts
        forM_ attrs $
          insert . uncurry (TimespanAttribute tid)
          . join (***) L.toStrict . first L.init
        return tid

postAttribute :: ConnectionPool -> ActionE ()
-- | 'postAttribute' sets or removes a 'TimespanAttribute' based on a request.
postAttribute p = do
  timespan <- paramE     "timespan"
  key      <- paramE     "key"
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
  name  <- paramE     "name"
  runDatabase p $
    let c = Clock name
    in  liftAE . jsonKey =<< case clock of
      Just i  -> let k = mkKey i
                 in  repsert k c >> return k
      Nothing -> insert c
