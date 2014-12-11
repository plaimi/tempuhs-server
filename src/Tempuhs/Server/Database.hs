{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

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
import qualified Database.Esqueleto as E
import Database.Esqueleto
  (
  from,
  isNothing,
  val,
  where_,
  )
import qualified Database.Esqueleto.Internal.Language
  (
  From,
  )
import Database.Persist
  (
  Entity,
  Key,
  PersistEntity,
  PersistValue (PersistInt64),
  (=.),
  getBy,
  keyFromValues,
  )
import Database.Persist.Class
  (
  EntityField,
  PersistField,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  SqlPersistT,
  runSqlPool,
  runMigration,
  )
import Database.Persist.Types
  (
  Update,
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
  c  <- liftAE $ paramE p
  mc <- getBy $ UniqueClock c
  case mc of
    Just d  -> return d
    Nothing -> liftAE $ raise $ errInvalidParam $ L.toStrict p

(=..) :: PersistField v => EntityField f v -> Maybe v -> [Update f]
-- | '(=..)' sets an @'EntityField' f v@, to the passed in 'Maybe' value,
-- which is a 'PersistField'. This returns an @'Update' f@ in the '[]'
-- 'Monad'. If the update is unreasonable (i.e. the passed in value is
-- 'Nothing'), '[]' is returned.
f =.. v = [f =. w | Just w <- [v]]

joinList :: forall (m :: * -> *) a b (expr :: * -> *) backend.
                   Database.Esqueleto.Internal.Language.From m expr backend a
         => [b -> a -> expr (E.Value Bool)] -> m b -> m b
-- | 'joinList' takes two lists of Esqueleto expressions. The first is a list
-- of expressions to SQL JOIN to every member of the second list, typically
-- a list of SQL SELECTs.
joinList (e:es) t = joinList es t >>= \t' -> from $ \b -> where_ (e t' b)
                                                       >> return t'
joinList []     t = t

cmpMaybe :: forall t (query :: * -> *) (expr :: * -> *)
                     backend (query1 :: * -> *) (expr1 :: * -> *)
                     backend1 typ.
                     (E.Esqueleto query1 expr1 backend1
                     ,E.Esqueleto query expr backend
                     ,E.PersistField typ
                     ,E.PersistField t)
          => (expr1 (E.Value (Maybe typ))
          -> expr (E.Value (Maybe t))
          -> expr1 (E.Value Bool))
          -> expr1 (E.Value (Maybe typ))
          -> Maybe t
          -> expr1 (E.Value Bool)
-- | If b is 'Just', 'cmpMaybe' applies the passed in function to a, and gets
-- the value of b. If b is 'Nothing', 'cmpMaybe' checks if a is 'Nothing'.
cmpMaybe f a b@(Just _) = f a $ val b
cmpMaybe _ a _          = isNothing a
