{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  User API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.User where

import Data.Foldable
  (
  toList,
  )
import Database.Persist
  (
  SelectOpt (Asc),
  (==.),
  insert,
  repsert,
  selectList,
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

postUser :: ConnectionPool -> ActionE ()
-- | 'postUser' inserts a new 'User' into the database, or replaces an
-- existing one, from a request.
postUser p = do
  u <- maybeParam "user"
  n <- paramE    "name"
  runDatabase p $
    let v = User n Nothing
    in  liftAE . jsonKey =<< case u of
      Just i  -> let k = mkKey i
                 in  repsert k v >> return k
      Nothing -> insert v

users :: ConnectionPool -> ActionE ()
-- | 'users' serves a request for a list of 'User's.
users p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [UserName ==. un       | un <- toList n] ++
                [UserId   ==. mkKey ui | ui <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc UserId]

deleteUser :: ConnectionPool -> ActionE ()
-- | 'deleteUser' updates the rubbish field of an existing 'User'.
deleteUser = nowow "user" UserRubbish
