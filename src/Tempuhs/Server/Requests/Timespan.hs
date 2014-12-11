{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Timespan API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Timespan where

import Control.Arrow
  (
  second,
  )
import Control.Monad
  (
  void,
  )
import qualified Database.Esqueleto as E
import Database.Esqueleto
  (
  (^.),
  (&&.),
  asc,
  orderBy,
  val,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  Key (Key),
  entityKey,
  insert,
  replace,
  update,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  json,
  )

import Plailude
import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  (=..),
  attributeSearch,
  clockParam,
  getAttrs,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.DELETE
  (
  nowow,
  owow,
  )
import Tempuhs.Server.Param
  (
  defaultParam,
  maybeParam,
  maybeUnwrap,
  paramE,
  rescueMissing,
  )
import Tempuhs.Server.Requests.Timespan.Attributes
  (
  attributesParams,
  updateAttribute,
  )
import Tempuhs.Server.Requests.Timespan.Util
  (
  clockFilter,
  descendantLookup,
  filters,
  idFilter,
  joinList,
  timeParams,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  )

timespans :: ConnectionPool -> ActionE ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans pool = do
  tid   <- maybeParam     "id"
  p     <- maybeParam     "parent"
  b     <- maybeParam     "begin"
  e     <- maybeParam     "end"
  r     <- maybeParam     "rubbish"
  ds    <- defaultParam 0 "descendants"
  joins <- attributeSearch
  runDatabase pool $ do
    c <- liftAE . rescueMissing =<< erretreat (clockParam "c")
    list <- E.select $
      joinList joins $
        E.from $ \t -> do
          E.where_ $ foldl (&&.) (val True) (clockFilter t c   ++
                                             filters t p r e b ++
                                             idFilter t (mkKey <$> tid))
          orderBy [asc $ t ^. TimespanId]
          return t
    descs <- descendantLookup (ceiling (ds :: Double)) (entityKey <$> list)
    liftAE . json =<< mapM (\a -> (,) a <$> getAttrs a) (list ++ descs)

postTimespan :: ConnectionPool -> ActionE ()
-- | 'postTimespan' inserts a 'Timespan'.
postTimespan pool = do
  p                             <- maybeParam     "parent"
  ((bMin, bMax), (eMin, eMax))  <- timeParams
  w                             <- defaultParam 1 "weight"
  r                             <- return Nothing
  as                            <- attributesParams
  runDatabase pool $ liftAE . jsonKey =<< do
    c   <- clockParam "clock"
    tid <- insert $ Timespan (mkKey <$> p) (entityKey c)
                             bMin bMax eMin eMax w r
    mapM_ (insert . uncurry (TimespanAttribute tid)) as
    return tid

replaceTimespan :: ConnectionPool -> ActionE ()
-- | 'replaceTimespan' replaces a 'Timespan'.
replaceTimespan pool = do
  t                             <- paramE         "timespan"
  p                             <- maybeParam     "parent"
  ((bMin, bMax), (eMin, eMax))  <- timeParams
  w                             <- defaultParam 1 "weight"
  r                             <- return Nothing
  as                            <- attributesParams
  runDatabase pool $ liftAE . jsonKey =<< do
    c      <- clockParam "clock"
    let tid = mkKey t
    replace tid $ Timespan (mkKey <$> p) (entityKey c) bMin bMax eMin eMax w r
    mapM_ (insert . uncurry (TimespanAttribute tid)) as
    return tid

deleteTimespan :: ConnectionPool -> ActionE ()
-- | 'deleteTimespan' updates the rubbish field of an existing 'Timespan'.
deleteTimespan = nowow "timespan" TimespanRubbish

unsafeDeleteTimespan :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteClock' hard-deletes a 'Timespan' from the database.
unsafeDeleteTimespan p =
  void $ (owow "timespan" p :: ActionE (Maybe (Key Timespan)))

patchTimespan :: ConnectionPool -> ActionE ()
-- | 'patchTimespan' modifies a 'Timespan'.
patchTimespan pool = do
  t      <- paramE     "timespan"
  p      <- maybeParam "parent"
  bMin   <- maybeParam "beginMin"
  bMax   <- maybeParam "beginMax"
  eMin   <- maybeParam "endMin"
  eMax   <- maybeParam "endMax"
  w      <- maybeParam "weight"
  as     <- attributesParams
  runDatabase pool $ liftAE . jsonKey =<< do
    c <- liftAE . rescueMissing =<< erretreat (clockParam "clock")
    let k = mkKey t
    update k $ concat [TimespanParent   =.. (fmap mkKey . maybeUnwrap <$> p)
                      ,TimespanClock    =.. (entityKey <$> c)
                      ,TimespanBeginMin =.. bMin
                      ,TimespanBeginMax =.. bMax
                      ,TimespanEndMin   =.. eMin
                      ,TimespanEndMax   =.. eMax
                      ,TimespanWeight   =.. w
                      ]
    mapM_ (uncurry (updateAttribute k) . second Just) as
    return k

patchAttribute :: ConnectionPool -> ActionE ()
-- | 'patchAttribute' sets or removes a 'TimespanAttribute'.
patchAttribute p = do
  t     <- paramE     "timespan"
  key   <- paramE     "key"
  value <- maybeParam "value"
  runDatabase p $ let k = mkKey t in updateAttribute k key value
