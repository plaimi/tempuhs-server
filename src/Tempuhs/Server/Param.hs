{- |
Module      :  $Header$
Description :  The tempuhs server parametre handling.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Param where

import Data.Maybe
  (
  fromMaybe,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Text.Lazy
  (
  Text,
  )
import Web.Scotty.Trans
  (
  Parsable,
  param,
  rescue,
  )

import Tempuhs.Server.Spock
  (
  ActionE,
  )

maybeParam :: Parsable a => Text -> ActionE (Maybe a)
-- | 'maybeParam' looks up a parametre and wraps it in a 'Maybe', returning
-- 'Nothing' if the parametre is not found.
maybeParam key = (Just <$> param key) `rescue` (\_ -> return Nothing)

defaultParam :: Parsable a => a -> Text -> ActionE a
-- | 'defaultParam' looks up a parametre and returns a default value if the
-- parametre is not found.
defaultParam d p = fromMaybe d <$> maybeParam p
