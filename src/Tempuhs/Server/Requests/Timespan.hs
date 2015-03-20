{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Timespan API.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Timespan where

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
  clockParam,
  joinList,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.DELETE
  (
  attributesNowow,
  owow,
  )
import Tempuhs.Server.Laws.Timespan
  (
  flexTimespan,
  parentCycle,
  )
import Tempuhs.Server.Param
  (
  defaultParam,
  maybeParam,
  maybeUnwrap,
  paramE,
  rescueMissing,
  )
import Tempuhs.Server.Requests.Attributes.Mono
  (
  getTimespanAttrs,
  updateTimespanAttributes,
  )
import Tempuhs.Server.Requests.Attributes.Poly
  (
  attributesParams,
  attributeSearch,
  insertAttributes,
  patchAttribute,
  )
import Tempuhs.Server.Requests.Timespan.Util
  (
  clockFilter,
  descendantLookup,
  filters,
  idFilter,
  timeParams,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  errInternal,
  errInvalidParam,
  jsonError,
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
  joins <- attributeSearch TimespanAttributeTimespan TimespanId
                           TimespanAttributeName TimespanAttributeValue
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
    liftAE . json =<< mapM (\a -> (,) a <$> getTimespanAttrs a)
                                                (list ++ descs)

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
    insertAttributes TimespanAttribute as tid
    flexTimespan tid
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
  runDatabase pool $ do
    let tid = mkKey t
        q   = mkKey <$> p
    c     <- clockParam "clock"
    pCycle <- case q of
                Just qq -> parentCycle [tid] qq
                _       -> return $ Just False
    case pCycle of
      Just False -> do
        replace tid $ Timespan q (entityKey c) bMin bMax eMin eMax w r
        insertAttributes TimespanAttribute as tid
        flexTimespan tid
        liftAE $ jsonKey tid
      Just True -> liftAE $ jsonError $ errInvalidParam "parent: cycle"
      Nothing   -> liftAE $ jsonError $ errInternal "database inconsistency"

deleteTimespan :: ConnectionPool -> ActionE ()
-- | 'deleteTimespan' updates the rubbish field of an existing 'Timespan'.
deleteTimespan p = attributesNowow "timespan"
                                   TimespanRubbish
                                   getTimespanAttrs
                                   timespanAttributeName
                                   updateTimespanAttributes
                                   p

unsafeDeleteTimespan :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteClock' hard-deletes a 'Timespan' from the database.
unsafeDeleteTimespan = owow "timespan" timespanRubbish

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
  runDatabase pool $ do
    c <- liftAE . rescueMissing =<< erretreat (clockParam "clock")
    let k = mkKey t
        q = fmap mkKey . maybeUnwrap <$> p
    pcycle <- case q of
                Just (Just r) -> parentCycle [k] r
                _             -> return $ Just False
    case pcycle of
       Just False -> do
         update k $ concat
           [TimespanParent   =.. q
           ,TimespanClock    =.. (entityKey <$> c)
           ,TimespanBeginMin =.. bMin
           ,TimespanBeginMax =.. bMax
           ,TimespanEndMin   =.. eMin
           ,TimespanEndMax   =.. eMax
           ,TimespanWeight   =.. w
           ]
         updateTimespanAttributes k as
         flexTimespan k
         liftAE $ jsonKey k
       Just True -> liftAE $ jsonError $ errInvalidParam "parent: cycle"
       Nothing   -> liftAE $ jsonError $ errInternal "database inconsistency"

patchTimespanAttribute :: ConnectionPool -> ActionE ()
-- | 'patchTimespanAttribute' sets or removes a 'TimespanAttribute'.
patchTimespanAttribute = patchAttribute "timespan" UniqueTimespanAttribute
                                                   TimespanAttributeValue
                                                   TimespanAttribute
                                                   TimespanAttributeRubbish
                                                   timespanAttributeRubbish
