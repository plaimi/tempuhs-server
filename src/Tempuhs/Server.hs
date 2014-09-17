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
  timespans,
  )
import Tempuhs.Server.POST
  (
  postAttribute,
  postClock,
  postTimespan,
  )
import Tempuhs.Server.DELETE
  (
  deleteTimespan,
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
  get    "/timespans"  $ timespans      dbPool
  get    "/clocks"     $ clocks         dbPool
  post   "/timespans"  $ postTimespan   dbPool
  post   "/clocks"     $ postClock      dbPool
  post   "/attributes" $ postAttribute  dbPool
  delete "/timespans"  $ deleteTimespan dbPool
  notFound $ jsonError errNotFound
