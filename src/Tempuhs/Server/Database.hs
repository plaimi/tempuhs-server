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
  KeyBackend (Key),
  PersistValue (PersistInt64),
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

import Tempuhs.Chronology
import Tempuhs.Server.Spock
  (
  ActionE,
  )

getAttrs :: Entity Timespan -> SqlPersistM [Entity TimespanAttribute]
-- | 'getAttrs' returns returns a list of all 'TimespanAttribute's for a given
-- 'Timespan'.
getAttrs e = selectList [TimespanAttributeTimespan ==. entityKey e] []

mkKey :: Integer -> KeyBackend backend entity
-- | 'mkKey' is a convenience function for constructing a database key.
mkKey = Key . PersistInt64 . fromInteger

runDatabase :: ConnectionPool -> SqlPersistM a -> ActionE a
-- | 'runDatabase' is a convenience function for running a database
-- transaction within an 'ActionE', taking care of migration if necessary.
runDatabase p a = liftIO $ runSqlPersistMPool (runMigration migrateAll >> a) p
