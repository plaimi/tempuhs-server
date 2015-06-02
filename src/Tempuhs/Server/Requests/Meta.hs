{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Metarequests regarding tempuhs itself.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Meta where

import Control.Monad.IO.Class
  (
  liftIO,
  )
import Web.Scotty.Trans
  (
  file,
  setHeader,
  )

import Tempuhs.Server.Spock
  (
  ActionE,
  )

import Paths_tempuhs_server

src :: ActionE ()
-- | 'src' serves the source code of the currently running tempuhs instance.
src = do
  setHeader "Content-Disposition" "attachment"
  setHeader "Content-Type" "application/x-gzip"
  setHeader "filename" "src.tar.gz"
  file =<< liftIO (getDataFileName "data/src.tar.gz")
