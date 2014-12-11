{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Permissionset API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Permissionset where

import Control.Monad
  (
  void,
  )
import Data.Foldable
  (
  toList,
  )
import Database.Persist
  (
  Key,
  SelectOpt (Asc),
  (==.),
  insert,
  replace,
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
  owow,
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

postPermissionset :: ConnectionPool -> ActionE ()
-- | 'postPermissionset' inserts a 'Permissionset'.
postPermissionset p = do
  tid <- paramE "timespan"
  rid <- paramE "role"
  o   <- paramE "own"
  r   <- paramE "read"
  w   <- paramE "write"
  runDatabase p $ let k = mkKey tid in liftAE . jsonKey =<<
                      insert (Permissionset k (mkKey rid) o r w Nothing)

deletePermissionset :: ConnectionPool -> ActionE ()
-- | 'deletePermissionset' updates the rubbish field of an existing
-- 'Permissionset'.
deletePermissionset = nowow "permissionset" PermissionsetRubbish

replacePermissionset :: ConnectionPool -> ActionE ()
-- | 'replacePermissionset' replaces a 'Permissionset'.
replacePermissionset p = do
  ps  <- paramE "permissionset"
  tid <- paramE "timespan"
  rid <- paramE "role"
  o   <- paramE "own"
  r   <- paramE "read"
  w   <- paramE "write"
  runDatabase p $ let k = mkKey ps in liftAE . jsonKey =<< do
    replace k (Permissionset (mkKey tid) (mkKey rid) o r w Nothing)
    return k

unsafeDeletePermissionset :: ConnectionPool -> ActionE ()
-- | 'unsafeDeletePermissionset' hard-deletes a 'Permissionset' from the
-- database.
unsafeDeletePermissionset p =
  void $ (owow "permissionset" p :: ActionE (Maybe (Key Permissionset)))

patchPermissionset :: ConnectionPool -> ActionE ()
-- | 'patchPermissionset' modifies a 'Permissionset'.
patchPermissionset p = do
  ps <- paramE     "permissionset"
  o  <- maybeParam "own"
  r  <- maybeParam "read"
  w  <- maybeParam "write"
  runDatabase p $ let k = mkKey ps in liftAE . jsonKey =<< do
    update k (concat [PermissionsetOwn   =.. o
                     ,PermissionsetRead  =.. r
                     ,PermissionsetWrite =.. w
                     ])
    return k
