{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Role API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Role where

import Data.Foldable
  (
  toList,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
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

roles :: ConnectionPool -> ActionE ()
-- | 'roles' serves a request for a list of 'Role's.
roles p = do
  n <- maybeParam "name"
  s <- maybeParam "namespace"
  r <- maybeParam "id"
  let filters = [RoleName      ==. rn       | rn <- toList n] ++
                [RoleNamespace ==. mkKey rs | rs <- toList s] ++
                [RoleId        ==. mkKey ri | ri <- toList r]
  runDatabase p $ liftAE . json =<< selectList filters [Asc RoleId]

postRole :: ConnectionPool -> ActionE ()
-- | 'postRole' inserts a new 'Role'.
postRole p = do
  n  <- paramE "name"
  ns <- paramE "namespace"
  runDatabase p $ liftAE . jsonKey =<< insert (Role n (mkKey ns) Nothing)

replaceRole :: ConnectionPool -> ActionE ()
-- | 'replaceRole' replaces a 'Role'.
replaceRole p = do
  r  <- paramE "role"
  n  <- paramE "name"
  ns <- paramE "namespace"
  runDatabase p $ let k = mkKey r in liftAE . jsonKey =<< do
    replace k (Role n (mkKey ns) Nothing)
    return k

deleteRole :: ConnectionPool -> ActionE ()
-- | 'deleteRole' updates the rubbish field of an existing 'Role'.
deleteRole = nowow "role" RoleRubbish

unsafeDeleteRole :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteRole' hard-deletes a 'Role' from the database.
unsafeDeleteRole = owow "role" roleRubbish

patchRole :: ConnectionPool -> ActionE ()
-- | 'patchRole' modifies a 'Role'.
patchRole p = do
  r  <- paramE     "role"
  n  <- maybeParam "name"
  ns <- maybeParam "namespace"
  runDatabase p $ let k = mkKey r in liftAE . jsonKey =<< do
    update k (concat [RoleName      =.. n
                     ,RoleNamespace =.. (mkKey <$> ns)
                     ])
    return k
