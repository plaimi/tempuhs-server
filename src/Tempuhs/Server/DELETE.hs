{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Module      :  $Header$
Description :  The tempuhs server DELETE API.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.DELETE where

import Control.Monad
  (
  void,
  )
import Control.Monad.IO.Class
  (
  liftIO,
  )
import Control.Monad.Trans.Reader
  (
  ReaderT,
  )
import Control.Monad.Trans.Resource.Internal
  (
  ResourceT,
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
  Entity (Entity),
  (=.),
  delete,
  entityVal,
  get,
  update,
  )
import Database.Persist.Class
  (
  EntityField,
  PersistEntity (Key),
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

owow :: (PersistEntity v, PersistEntityBackend v ~ SqlBackend)
     => Text -> (v -> Maybe UTCTime) -> ConnectionPool
     -> ActionE ()
-- | 'owow' takes a parametre to look up in a 'ConnectionPool', and a function
-- to retrieve the rubbish field of the resource looked up with the parametre.
-- The resource is then deleted if it is rubbish per the passed in rubbish
-- field function. It inflicts owow in the form of *hard deleting* the row. If
-- it doesn't exist, or isn't rubbish, an error on the parametre is raised per
-- 'withParam'.
owow p f c =
  withParam p $ \q -> runDatabase c $ do
    let k = mkKey q
    mr <- get k
    case f <$> mr of
      Just (Just _) -> return <$> (delete k >> liftAE jsonSuccess)
      _             -> return Nothing

attributesNowow :: (PersistEntity v, PersistEntityBackend v ~ SqlBackend)
                => Text
                -> EntityField v (Maybe UTCTime)
                -> (Entity v -> ReaderT SqlBackend (ResourceT ActionE) [Entity t])
                -> (t -> a)
                -> (Key v -> [(a, Maybe r)] -> ReaderT SqlBackend (ResourceT ActionE) u)
                -> ConnectionPool
                -> ActionE ()
-- | 'attributesNowow' takes a parametre to look up. If the row exists, it
-- sets the passed in field and its attributes to 'getCurrentTime'. If not, an
-- error on the parametre is raised per 'withParam'.
attributesNowow p f ga an ua c =
  withParam p $ \r -> do
    now <- liftIO getCurrentTime
    runDatabase c $ do
      let k = mkKey r
      mr <- get k
      case mr of
        Just s  -> do
          as <- ga (Entity k s)
          let bs = map (an . entityVal) as
          void $ ua k $ zip bs (repeat Nothing)
          update k [f =. Just now]
          return <$> liftAE jsonSuccess
        Nothing -> return Nothing
