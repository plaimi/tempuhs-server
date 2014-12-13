{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Requests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests where

import Database.Persist.Sql
  (
  ConnectionPool,
  )

import Web.Scotty.Trans
  (
  delete,
  get,
  patch,
  post,
  put,
  )

import Tempuhs.Server.Requests.Clock
  (
  clocks,
  deleteClock,
  patchClock,
  postClock,
  replaceClock,
  unsafeDeleteClock,
  )
import Tempuhs.Server.Requests.Permissionset
  (
  deletePermissionset,
  patchPermissionset,
  permissionsets,
  postPermissionset,
  unsafeDeletePermissionset,
  )
import Tempuhs.Server.Requests.Role
  (
  deleteRole,
  patchRole,
  postRole,
  replaceRole,
  roles,
  unsafeDeleteRole,
  )
import Tempuhs.Server.Requests.Timespan
  (
  deleteTimespan,
  patchTimespan,
  patchTimespanAttribute,
  postTimespan,
  replaceTimespan,
  timespans,
  unsafeDeleteTimespan,
  )
import Tempuhs.Server.Requests.User
  (
  users,
  deleteUser,
  patchUser,
  patchUserAttribute,
  postUser,
  replaceUser,
  unsafeDeleteUser,
  )
import Tempuhs.Server.Spock
  (
  ScottyE,
  )


clockRequests :: ConnectionPool -> ScottyE ()
clockRequests p = do
  get    "/clocks"       $ clocks            p
  post   "/clocks"       $ postClock         p
  put    "/clocks"       $ replaceClock      p
  delete "/clocks"       $ deleteClock       p
  delete "/clocks/purge" $ unsafeDeleteClock p
  patch  "/clocks"       $ patchClock        p

permissionsetRequests :: ConnectionPool -> ScottyE ()
permissionsetRequests p = do
  get    "/permissionsets"       $ permissionsets            p
  post   "/permissionsets"       $ postPermissionset         p
  delete "/permissionsets"       $ deletePermissionset       p
  delete "/permissionsets/purge" $ unsafeDeletePermissionset p
  patch  "/permissionsets"       $ patchPermissionset        p

roleRequests :: ConnectionPool -> ScottyE ()
roleRequests p = do
  get    "/roles"       $ roles            p
  post   "/roles"       $ postRole         p
  put    "/roles"       $ replaceRole      p
  delete "/roles"       $ deleteRole       p
  delete "/roles/purge" $ unsafeDeleteRole p
  patch  "/roles"       $ patchRole        p

timespanRequests :: ConnectionPool -> ScottyE ()
timespanRequests p = do
  get    "/timespans"          $ timespans              p
  post   "/timespans"          $ postTimespan           p
  put    "/timespans"          $ replaceTimespan        p
  delete "/timespans"          $ deleteTimespan         p
  delete "/timespans/purge"    $ unsafeDeleteTimespan   p
  patch  "/timespans"          $ patchTimespan          p
  patch  "/timespanAttributes" $ patchTimespanAttribute p

userRequests :: ConnectionPool -> ScottyE ()
userRequests p = do
  get    "/users"          $ users             p
  post   "/users"          $ postUser          p
  put    "/users"          $ replaceUser       p
  delete "/users"          $ deleteUser        p
  delete "/users/purge"    $ unsafeDeleteUser  p
  patch  "/users"          $ patchUser         p
  patch  "/userAttributes" $ patchUserAttribute p
