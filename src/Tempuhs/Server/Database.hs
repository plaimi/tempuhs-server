{- |
Module      :  $Header$
Description :  The tempuhs server database functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Database where

import Control.Monad.Trans.Class
  (
  lift,
  )
import Control.Monad.Trans.Resource
  (
  ResourceT,
  runResourceT,
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Database.Persist
  (
  Entity,
  Key,
  PersistEntity,
  PersistValue (PersistInt64),
  SelectOpt (Asc),
  (==.),
  entityKey,
  getBy,
  keyFromValues,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  SqlPersistT,
  runSqlPool,
  runMigration,
  )
import Web.Scotty.Trans
  (
  raise,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Param
  (
  paramE,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  errInvalidParam,
  )

-- | A 'SqlPersistA' is a 'SqlPersistT' that can be run within an 'ActionE'.
type SqlPersistA = SqlPersistT (ResourceT ActionE)

runSqlPersistAPool :: SqlPersistA a -> ConnectionPool -> ActionE a
-- | 'runSqlPersistAPool' runs a database transaction within an 'ActionE',
-- using a connection from the given 'ConnectionPool'.
runSqlPersistAPool x pool = runResourceT $ runSqlPool x pool

getAttrs :: Entity Timespan -> SqlPersistA [Entity TimespanAttribute]
-- | 'getAttrs' returns a list of all 'TimespanAttribute's for a given
-- 'Timespan'.
getAttrs e = selectList [TimespanAttributeTimespan ==. entityKey e]
                        [Asc TimespanAttributeId]

mkKey :: PersistEntity record => Integer -> Key record
-- | 'mkKey' is a convenience function for constructing a database key.
mkKey = either (error . T.unpack) id .
        keyFromValues . return . PersistInt64 . fromInteger

runDatabase :: ConnectionPool -> SqlPersistA a -> ActionE a
-- | 'runDatabase' is a convenience function for running a database
-- transaction within an 'ActionE', taking care of migration if necessary.
runDatabase p a = runSqlPersistAPool (runMigration migrateAll >> a) p

liftAE :: ActionE a -> SqlPersistA a
-- | 'liftAE' lifts a computation from the 'ActionE' monad into the
-- 'SqlPersistA' monad.
liftAE = lift . lift

clockParam :: L.Text -> SqlPersistA (Entity Clock)
-- | 'clockParam' looks up an @'Entity' 'Clock'@ from the parametre of the
-- given name, raising 'errInvalidParam' if there is no match.
clockParam p = do
  clock <- liftAE $ paramE p
  maybeClock <- getBy $ UniqueClock clock
  case maybeClock of
    Just c  -> return c
    Nothing -> liftAE $ raise $ errInvalidParam $ L.toStrict p
