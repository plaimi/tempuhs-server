{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Permissionset API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Permissionset where

import Data.Foldable
  (
  toList,
  )
import Database.Persist
  (
  SelectOpt (Asc),
  (==.),
  insert,
  selectList,
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

import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  (=..),
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.DELETE
  (
  nowow,
  )
import Tempuhs.Server.Param
  (
  maybeParam,
  paramE,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  )

postPermissionset :: ConnectionPool -> ActionE ()
-- | 'postPermissionset' inserts a 'Permissionset' into the database, or
-- updates an existing one, from a request.
postPermissionset p = do
  ps <- maybeParam "permissionset"
  runDatabase p $
    liftAE . jsonKey =<<
      case ps of
        Just i -> do
          let k = mkKey i
          o   <- liftAE $ maybeParam "own"
          r   <- liftAE $ maybeParam "read"
          w   <- liftAE $ maybeParam "write"
          update (mkKey i) $ concat [PermissionsetOwn      =.. o
                                    ,PermissionsetRead     =.. r
                                    ,PermissionsetWrite    =.. w
                                    ]
          return k
        Nothing -> do
          tid <- liftAE $ paramE "timespan"
          rid <- liftAE $ paramE "role"
          o   <- liftAE $ paramE "own"
          r   <- liftAE $ paramE "read"
          w   <- liftAE $ paramE "write"
          return =<< insert $ Permissionset (mkKey tid) (mkKey rid) o r w
                                            Nothing

permissionsets :: ConnectionPool -> ActionE ()
-- | 'permissionsets' serves a request for a list of 'Permissionset's.
permissionsets p = do
  ps <- maybeParam "id"
  t  <- maybeParam "timespan"
  r  <- maybeParam "role"
  let filters = [PermissionsetId       ==. mkKey pss | pss <- toList ps] ++
                [PermissionsetTimespan ==. mkKey ti  | ti  <- toList t ] ++
                [PermissionsetRole     ==. mkKey ri  | ri  <- toList r ]
  runDatabase p $ liftAE . json =<< selectList filters [Asc PermissionsetId]

deletePermissionset :: ConnectionPool -> ActionE ()
-- | 'deletePermissionset' updates the rubbish field of an existing
-- 'Permissionset'.
deletePermissionset = nowow "permissionset" PermissionsetRubbish
