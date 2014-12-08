{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Role API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Role where

import Control.Monad
  (
  void,
  )
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
  Key,
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

postRole :: ConnectionPool -> ActionE ()
-- | 'postRole' inserts a new 'Role' into the database, or updates an
-- existing one, from a request.
postRole p = do
  r <- maybeParam "role"
  runDatabase p $
    liftAE . jsonKey =<<
      case r of
        Just i -> do
          let k = mkKey i
          n  <- liftAE $ maybeParam "name"
          ns <- liftAE $ maybeParam "namespace"
          update (mkKey i) $ concat [RoleName      =.. n
                                    ,RoleNamespace =.. (mkKey <$> ns)
                                    ]
          return k
        Nothing -> do
          n  <- liftAE $ paramE "name"
          ns <- liftAE $ paramE "namespace"
          return =<< insert $ Role n (mkKey ns) Nothing

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

deleteRole :: ConnectionPool -> ActionE ()
-- | 'deleteRole' updates the rubbish field of an existing 'Role'.
deleteRole = nowow "role" RoleRubbish

unsafeDeleteRole :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteRole' hard-deletes a 'Role' from the database.
unsafeDeleteRole p = void $ (owow "role" p :: ActionE (Maybe (Key Role)))
