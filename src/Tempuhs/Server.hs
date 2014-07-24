{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs web server application
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server where

import Control.Monad
  (join
  ,liftM)
import Control.Monad.IO.Class
  (liftIO)
import Data.Maybe
  (fromMaybe)
import Data.Text.Lazy
  (Text,pack)
import Database.Persist
  (Entity (Entity)
  ,(<=.)
  ,(==.)
  ,(>=.)
  ,entityKey
  ,getBy
  ,insert
  ,selectList)
import Database.Persist.Sql
  (ConnectionPool
  ,SqlPersistM
  ,runMigration
  ,runSqlPersistMPool)
import Web.Scotty
  (ActionM
  ,Parsable
  ,ScottyM
  ,get
  ,param
  ,post
  ,rescue
  ,text)

import Tempuhs.Chronology

maybeParam :: Parsable a => Text -> ActionM (Maybe a)
-- | 'maybeParam' looks up a parameter and wraps it in a 'Maybe', returning
-- 'Nothing' if the parameter is not found.
maybeParam key = (param key >>= return . Just) `rescue` (\_ -> return Nothing)

defaultParam :: Parsable a => a -> Text -> ActionM a
-- | 'defaultParam' looks up a parameter and returns a default value if the
-- parameter is not found.
defaultParam val = liftM (fromMaybe val) . maybeParam

runDatabase :: ConnectionPool -> SqlPersistM a -> ActionM a
-- | 'runDatabase' is a convenience function for running a database
-- transaction within an 'ActionM', taking care of migration if necessary.
runDatabase p a =
  liftIO $ runSqlPersistMPool (runMigration migrateAll >> a) p

getAttrs :: Entity Timespan -> SqlPersistM [Entity TimespanAttribute]
-- | 'getAttrs' returns returns a list of all 'TimespanAttribute's for a given
-- 'Timespan'.
getAttrs e = selectList [TimespanAttributeTimespan ==. entityKey e] []

timespans :: ConnectionPool -> ActionM ()
-- | 'timespans' serves a basic request for a list of 'Timespan's and
-- associated 'TimespanAttribute's.
timespans p = do
  clock <- param "clock"
  begin <- param "begin"
  end   <- param "end"
  join $ runDatabase p $ do
    maybeClock <- getBy $ UniqueClock clock
    case maybeClock of
      Just (Entity clockKey _) -> do
        list <- selectList [TimespanClock ==. clockKey
                           ,TimespanBeginMin <=. end
                           ,TimespanEndMax >=. begin] []
        ts <- mapM (\e -> liftM ((,) e) (getAttrs e)) list
        return $ text . pack . show $ ts
      Nothing                  -> return $ text ""

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
        k <- insert $ Timespan (liftM read parent) clockKey
                      beginMin beginMax endMin endMax weight
        return $ text . pack . show $ k
      Nothing                  -> return $ text ""

postClock :: ConnectionPool -> ActionM ()
-- | 'postClock' inserts a new 'Clock' into the database from a request.
postClock p = do
  name <- param "name"
  join $ runDatabase p $ do
    k <- insert $ Clock name
    return $ text . pack . show $ k

serve :: ConnectionPool -> ScottyM ()
-- | 'serve' is the scotty application for tempuhs.
serve dbPool = do
  get  "/timespans" $ timespans dbPool
  post "/timespans" $ postTimespan dbPool
  post "/clocks"    $ postClock dbPool
