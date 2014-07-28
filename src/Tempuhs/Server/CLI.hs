{- |
Module      :  $Header$
Description :  The tempuhs server command line interface
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.CLI where

import Data.ByteString.Char8
  (
  pack,
  )
import Database.Persist.Postgresql
  (
  withPostgresqlPool,
  )
import Options.Applicative
  (
  Parser,
  (<>),
  (<$>),
  (<*>),
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
  )
import Web.Scotty
  (
  scotty,
  )

import Tempuhs.Server
  (
  serve,
  )

-- | A 'Config' stores all options needed by the program.
data Config = Config
  {database    :: String -- ^ Connection string for the database.
  ,connections :: Int    -- ^ Number of connections to the database.
  ,port        :: Int    -- ^ Server port.
  }

options :: Parser Config
-- | 'options' is a command-line 'Parser' for 'Config' options.
options = Config
  <$> strOption
      ( long    "database"
     <> short   'd'
     <> metavar "CONNSTR"
     <> help    "Connection string for the database" )
  <*> option
      ( long    "connections"
     <> short   'c'
     <> metavar "NUM"
     <> value   1
     <> help    "Number of connections to the database" )
  <*> option
      ( long    "port"
     <> short   'p'
     <> metavar "PORT"
     <> value   3000
     <> help    "Server port" )

run :: Config -> IO ()
-- | 'run' starts the server with the specified 'Config' options.
run (Config d c p) = withPostgresqlPool (pack d) c $ scotty p . serve

cli :: IO ()
-- | 'cli' starts the server with options from the command line.
cli = execParser opts >>= run
  where opts =
          info (helper <*> options)
          ( fullDesc
         <> progDesc "The server for the tempuhs chronicler"
         <> header   "tempuhs server" )
