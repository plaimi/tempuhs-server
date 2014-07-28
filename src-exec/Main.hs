{- |
Module      :  $Header$
Description :  The tempuhs server executable
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main where

import Tempuhs.Server.CLI
  (
  cli,
  )

main :: IO ()
-- | 'main' starts the server with options from the command line.
main = cli
