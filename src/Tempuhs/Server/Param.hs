{- |
Module      :  $Header$
Description :  The tempuhs server paramter handling.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Param where

import Control.Monad
  (
  liftM,
  )
import Data.Maybe
  (
  fromMaybe,
  )
import Data.Text.Lazy
  (
  Text,
  )
import Web.Scotty
  (
  ActionM,
  Parsable,
  param,
  rescue,
  )

maybeParam :: Parsable a => Text -> ActionM (Maybe a)
-- | 'maybeParam' looks up a parameter and wraps it in a 'Maybe', returning
-- 'Nothing' if the parameter is not found.
maybeParam key = (param key >>= return . Just) `rescue` (\_ -> return Nothing)

defaultParam :: Parsable a => a -> Text -> ActionM a
-- | 'defaultParam' looks up a parameter and returns a default value if the
-- parameter is not found.
defaultParam val = liftM (fromMaybe val) . maybeParam