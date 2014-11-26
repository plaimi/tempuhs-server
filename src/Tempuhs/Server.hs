{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs web server application
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server where

import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  defaultHandler,
  delete,
  get,
  notFound,
  post,
  )
import Tempuhs.Server.GET
  (
  clocks,
  permissionsets,
  roles,
  timespans,
  users,
  )
import Tempuhs.Server.POST
  (
  postAttribute,
  postClock,
  postPermissionsets,
  postRole,
  postTimespan,
  postUser,
  )
import Tempuhs.Server.DELETE
  (
  deleteRole,
  deleteTimespan,
  deleteUser,
  deletePermissionsets,
  )
import Tempuhs.Server.Spock
  (
  ScottyE,
  errNotFound,
  jsonError,
  )

serve :: ConnectionPool -> ScottyE ()
-- | 'serve' is the scotty application for tempuhs.
serve dbPool = do
  defaultHandler jsonError
  get    "/timespans"      $ timespans            dbPool
  get    "/clocks"         $ clocks               dbPool
  get    "/users"          $ users                dbPool
  get    "/roles"          $ roles                dbPool
  get    "/permissionsets" $ permissionsets       dbPool
  post   "/timespans"      $ postTimespan         dbPool
  post   "/attributes"     $ postAttribute        dbPool
  post   "/clocks"         $ postClock            dbPool
  post   "/users"          $ postUser             dbPool
  post   "/roles"          $ postRole             dbPool
  post   "/permissionsets" $ postPermissionsets   dbPool
  delete "/timespans"      $ deleteTimespan       dbPool
  delete "/users"          $ deleteUser           dbPool
  delete "/roles"          $ deleteRole           dbPool
  delete "/permissionsets" $ deletePermissionsets dbPool
  notFound $ jsonError errNotFound
