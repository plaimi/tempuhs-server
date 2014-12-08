{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Module      :  $Header$
Description :  The tempuhs server DELETE API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.DELETE where

import Control.Monad.IO.Class
  (
  liftIO,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Text.Lazy
  (
  Text,
  )
import Data.Time.Clock
  (
  UTCTime,
  getCurrentTime,
  )
import Database.Persist
  (
  (=.),
  delete,
  get,
  update,
  )
import Database.Persist.Class
  (
  EntityField,
  Key,
  PersistEntity,
  PersistEntityBackend,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  SqlBackend,
  )

import Tempuhs.Server.Database
  (
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  withParam,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonSuccess,
  )

nowow :: (PersistEntity v, PersistEntityBackend v ~ SqlBackend)
       => Text
       -> EntityField v (Maybe UTCTime)
       -> ConnectionPool
       -> ActionE ()
-- | 'nowow' takes a parametre to look up. If the row exists, it sets the
-- passed in field to 'getCurrentTime'. If not, an error on the parametre is
-- raised per 'withParam'.
nowow p f c =
  withParam p $ \r -> do
    now <- liftIO getCurrentTime
    runDatabase c $ do
      let k = mkKey r
      mr <- get k
      case mr of
        Just _  -> return <$> (update k [f =. Just now] >> liftAE jsonSuccess)
        Nothing -> return Nothing

owow :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
     => Text -> ConnectionPool -> ActionE (Maybe (Key a))
-- | 'owow' takes a parametre to look up. If the row exists, it inflicts owow
-- in the form of *hard deleting* the row. If it doesn't exist, an error on
-- the parametre is raised per 'withParam'.
owow p c =
  withParam p $ \r -> runDatabase c $ do
    let k = mkKey r
    mr <- get k
    case mr of
      Just _  -> return <$> (delete k >> liftAE jsonSuccess >>
                             return (Just k))
      Nothing -> return Nothing
