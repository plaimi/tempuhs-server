{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  User API.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.User where

import Data.Foldable
  (
  toList,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Esqueleto
  (
  (==.),
  (^.),
  (&&.),
  asc,
  from,
  orderBy,
  select,
  val,
  where_,
  )
import Database.Persist
  (
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

import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  (=..),
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
import Tempuhs.Server.Param
  (
  maybeParam,
  paramE,
  )
import Tempuhs.Server.Requests.Attributes.Mono
  (
  getUserAttrs,
  updateUserAttributes,
  )
import Tempuhs.Server.Requests.Attributes.Poly
  (
  attributesParams,
  attributeSearch,
  insertAttributes,
  patchAttribute,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  )

users :: ConnectionPool -> ActionE ()
-- | 'users' serves a request for a 'User' with their associated
-- 'UserAttribute's.
users p = do
  n  <- maybeParam "name"
  i  <- maybeParam "id"
  as <- attributeSearch UserAttributeUser UserId UserAttributeName
                                                 UserAttributeValue
  runDatabase p $ do
    usr <- select $
      joinList as $
          from $ \u -> do
            where_ $ foldl (&&.) (val True) $
                    [u ^. UserId   ==. val (mkKey ui) | ui <- toList i] ++
                    [u ^. UserName ==. val un         | un <- toList n]
            orderBy [asc $ u ^. UserId]
            return u
    liftAE . json =<< mapM (\a -> (,) a <$> getUserAttrs a) usr

postUser :: ConnectionPool -> ActionE ()
-- | 'postUser' inserts a 'User'.
postUser p = do
  n <- paramE "name"
  as <- attributesParams
  runDatabase p $ liftAE . jsonKey =<< do
    k <- insert (User n Nothing)
    insertAttributes UserAttribute as k
    return k

replaceUser :: ConnectionPool -> ActionE ()
-- | 'replaceUser' replaces a 'User'.
replaceUser p = do
  u  <- paramE "user"
  n  <- paramE "name"
  as <- attributesParams
  runDatabase p $ liftAE . jsonKey =<< let k = mkKey u in do
    replace k (User n Nothing)
    insertAttributes UserAttribute as k
    return k

deleteUser :: ConnectionPool -> ActionE ()
-- | 'deleteTimespan' updates the rubbish field of an existing 'Timespan'.
deleteUser p = attributesNowow "user"
                               UserRubbish
                               getUserAttrs
                               userAttributeName
                               updateUserAttributes
                               p

unsafeDeleteUser :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteUser' hard-deletes a 'User' from the database.
unsafeDeleteUser = owow "user" userRubbish

patchUser :: ConnectionPool -> ActionE ()
-- | 'patchUser' modifies a 'User'.
patchUser p = do
  u <- paramE     "user"
  n <- maybeParam "name"
  as <- attributesParams
  runDatabase p $ let k = mkKey u in do
    update k $ concat [UserName =.. n]
    updateUserAttributes k as
    liftAE $ jsonKey k

patchUserAttribute :: ConnectionPool -> ActionE ()
-- | 'patchUserAttribute' sets or removes a 'UserAttribute'.
patchUserAttribute = patchAttribute "user" UniqueUserAttribute
                                           UserAttributeValue
                                           UserAttribute
                                           UserAttributeRubbish
                                           userAttributeRubbish
