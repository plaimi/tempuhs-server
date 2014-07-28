{- |
Module      :  $Header$
Description :  The tempuhs server database functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Database where

import Control.Monad.IO.Class
  (
  liftIO,
  )
import Database.Persist
  (
  Entity,
  (==.),
  entityKey,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  SqlPersistM,
  runMigration,
  runSqlPersistMPool,
  )
import Web.Scotty
  (
  ActionM,
  )

import Tempuhs.Chronology

getAttrs :: Entity Timespan -> SqlPersistM [Entity TimespanAttribute]
-- | 'getAttrs' returns returns a list of all 'TimespanAttribute's for a given
-- 'Timespan'.
getAttrs e = selectList [TimespanAttributeTimespan ==. entityKey e] []

runDatabase :: ConnectionPool -> SqlPersistM a -> ActionM a
-- | 'runDatabase' is a convenience function for running a database
-- transaction within an 'ActionM', taking care of migration if necessary.
runDatabase p a =
  liftIO $ runSqlPersistMPool (runMigration migrateAll >> a) p
