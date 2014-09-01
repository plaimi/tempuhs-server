{- |
Module      :  $Header$
Description :  The tempuhs server database functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Database where

import Control.Monad.Logger
  (
  NoLoggingT,
  runNoLoggingT,
  )
import Control.Monad.Trans.Class
  (
  lift,
  )
import Control.Monad.Trans.Resource
  (
  ResourceT,
  runResourceT,
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
  SqlPersistT,
  runSqlPool,
  runMigration,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Spock
  (
  ActionE,
  )

-- | A 'SqlPersistA' is a 'SqlPersistT' that can be run within an 'ActionE'.
type SqlPersistA = SqlPersistT (NoLoggingT (ResourceT ActionE))

runSqlPersistAPool :: SqlPersistA a -> ConnectionPool -> ActionE a
-- | 'runSqlPersistAPool' runs a database transaction within an 'ActionE',
-- using a connection from the given 'ConnectionPool'.
runSqlPersistAPool x pool = runResourceT $ runNoLoggingT $ runSqlPool x pool

getAttrs :: Entity Timespan -> SqlPersistA [Entity TimespanAttribute]
-- | 'getAttrs' returns returns a list of all 'TimespanAttribute's for a given
-- 'Timespan'.
getAttrs e = selectList [TimespanAttributeTimespan ==. entityKey e] []

mkKey :: Integer -> KeyBackend backend entity
-- | 'mkKey' is a convenience function for constructing a database key.
mkKey = Key . PersistInt64 . fromInteger

runDatabase :: ConnectionPool -> SqlPersistA a -> ActionE a
-- | 'runDatabase' is a convenience function for running a database
-- transaction within an 'ActionE', taking care of migration if necessary.
runDatabase p a = runSqlPersistAPool (runMigration migrateAll >> a) p

liftAE :: ActionE a -> SqlPersistA a
-- | 'liftAE' lifts a computation from the 'ActionE' monad into the
-- 'SqlPersistA' monad.
liftAE = lift . lift . lift
