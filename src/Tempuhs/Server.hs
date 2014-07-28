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
import Web.Scotty
  (
  ScottyM,
  get,
  post,
  )

import Tempuhs.Server.GET
  (
  timespans,
  )
import Tempuhs.Server.POST
  (
  postAttribute,
  postClock,
  postTimespan,
  )

serve :: ConnectionPool -> ScottyM ()
-- | 'serve' is the scotty application for tempuhs.
serve dbPool = do
  get  "/timespans"  $ timespans dbPool
  post "/timespans"  $ postTimespan dbPool
  post "/clocks"     $ postClock dbPool
  post "/attributes" $ postAttribute dbPool
