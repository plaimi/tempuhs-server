{- |
Module      :  $Header$
Description :  The tempuhs server command line interface
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.CLI where

import Control.Monad.Logger
  (
  NoLoggingT (NoLoggingT),
  runNoLoggingT,
  )
import Data.ByteString.Char8
  (
  pack,
  )
import Database.Persist.Postgresql
  (
  withPostgresqlPool,
  )
import Network.Wai.Handler.Warp
  (
  Port,
  )
import Options.Applicative
  (
  Parser,
  (<>),
  (<$>),
  (<*>),
  auto,
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

import Tempuhs.Server
  (
  serve,
  )
import Tempuhs.Server.Spock
  (
  scottyE,
  )

-- | A 'Config' stores all the options needed by the program.
data Config = MkConfig
  {database    :: String -- ^ Connection string for the database.
  ,connections :: Int    -- ^ Number of connections to the database.
  ,port        :: Port   -- ^ Server port.
  }

options :: Parser Config
-- | 'options' is a command-line 'Parser' for 'Config' options.
options = MkConfig
  <$> strOption
      ( long    "database"
     <> short   'd'
     <> metavar "CONNSTR"
     <> help    "Connection string for the database" )
  <*> option auto
      ( long    "connections"
     <> short   'c'
     <> metavar "NUM"
     <> value   1
     <> help    "Number of connections to the database" )
  <*> option auto
      ( long    "port"
     <> short   'p'
     <> metavar "PORT"
     <> value   3000
     <> help    "Server port" )

run :: Config -> IO ()
-- | 'run' starts the server with the specified 'Config' options.
run (MkConfig d c p) = runNoLoggingT $
  withPostgresqlPool (pack d) c $ NoLoggingT . scottyE p . serve

cli :: IO ()
-- | 'cli' starts the server with options from the command line.
cli = execParser opts >>= run
  where opts =
          info (helper <*> options)
          ( fullDesc
         <> progDesc "The server for the tempuhs chronicler"
         <> header   "tempuhs server" )
