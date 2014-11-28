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
  post,
  )

import Tempuhs.Server.Requests.Clock
  (
  clocks,
  postClock,
  )
import Tempuhs.Server.Requests.Permissionset
  (
  permissionsets,
  deletePermissionset,
  postPermissionset,
  )
import Tempuhs.Server.Requests.Role
  (
  roles,
  deleteRole,
  postRole,
  )
import Tempuhs.Server.Requests.Timespan
  (
  deleteTimespan,
  postAttribute,
  postTimespan,
  timespans,
  )
import Tempuhs.Server.Requests.User
  (
  users,
  deleteUser,
  postUser,
  )
import Tempuhs.Server.Spock
  (
  ScottyE,
  )


clockRequests :: ConnectionPool -> ScottyE ()
clockRequests p = do
  get  "/clocks" $ clocks    p
  post "/clocks" $ postClock p

permissionsetRequests :: ConnectionPool -> ScottyE ()
permissionsetRequests p = do
  post   "/permissionsets" $ postPermissionset   p
  get    "/permissionsets" $ permissionsets      p
  delete "/permissionsets" $ deletePermissionset p

roleRequests :: ConnectionPool -> ScottyE ()
roleRequests p = do
  post   "/roles" $ postRole   p
  get    "/roles" $ roles      p
  delete "/roles" $ deleteRole p

timespanRequests :: ConnectionPool -> ScottyE ()
timespanRequests p = do
  post   "/timespans"  $ postTimespan   p
  get    "/timespans"  $ timespans      p
  delete "/timespans"  $ deleteTimespan p
  post   "/attributes" $ postAttribute  p

userRequests :: ConnectionPool -> ScottyE ()
userRequests p = do
  post   "/users" $ postUser   p
  get    "/users" $ users      p
  delete "/users" $ deleteUser p
