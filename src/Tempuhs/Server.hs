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
  notFound,
  )

import Tempuhs.Server.Requests
  (
  clockRequests,
  permissionsetRequests,
  roleRequests,
  timespanRequests,
  userRequests,
  )
import Tempuhs.Server.Spock
  (
  ScottyE,
  errNotFound,
  jsonError,
  )

serve :: ConnectionPool -> ScottyE ()
-- | 'serve' is the scotty application for tempuhs.
serve p = do
  defaultHandler jsonError
  clockRequests         p
  permissionsetRequests p
  roleRequests          p
  timespanRequests      p
  userRequests          p
  notFound $ jsonError errNotFound
