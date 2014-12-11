{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  User API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.User where

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
  (=.),
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

users :: ConnectionPool -> ActionE ()
-- | 'users' serves a request for a list of 'User's.
users p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [UserName ==. un       | un <- toList n] ++
                [UserId   ==. mkKey ui | ui <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc UserId]

postUser :: ConnectionPool -> ActionE ()
-- | 'postUser' inserts a 'User'.
postUser p = do
  n <- paramE "name"
  runDatabase p $ liftAE . jsonKey =<< insert (User n Nothing)

replaceUser :: ConnectionPool -> ActionE ()
-- | 'replaceUser' replaces a 'User'.
replaceUser p = do
  u <- paramE "user"
  n <- paramE "name"
  runDatabase p $ let k = mkKey u in liftAE . jsonKey =<< do
    replace k (User n Nothing)
    return k

deleteUser :: ConnectionPool -> ActionE ()
-- | 'deleteUser' updates the rubbish field of an existing 'User'.
deleteUser = nowow "user" UserRubbish

unsafeDeleteUser :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteUser' hard-deletes a 'User' from the database.
unsafeDeleteUser p = void $ (owow "user" p :: ActionE (Maybe (Key User)))

patchUser :: ConnectionPool -> ActionE ()
-- | 'patchUser' modifies a 'User'.
patchUser p = do
  u <- paramE "user"
  n <- paramE "name"
  runDatabase p $ let k = mkKey u in liftAE . jsonKey =<< do
    update k [UserName =. n]
    return k
