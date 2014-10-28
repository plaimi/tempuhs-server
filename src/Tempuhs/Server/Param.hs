{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server parametre handling.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Param where

import Control.Monad
  (
  msum,
  )
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
  pack,
  unpack,
  toStrict,
  )
import Data.Time.Format
  (
  ParseTime,
  parseTime,
  )
import System.Locale
  (
  defaultTimeLocale,
  )
import Web.Scotty.Trans
  (
  Parsable,
  params,
  parseParam,
  raise,
  rescue,
  )

import Tempuhs.Server.Spock
  (
  ActionE,
  errInvalidParam,
  errMissingParam,
  errorCode,
  )

-- | 'ParsableWrapper' is an ADT wrapper that permits us to write unorphaned
-- 'Parsable' instances.
data ParsableWrapper a = ParsableWrap
  {parsableUnwrap :: a -- ^ Unwrap the value.
  }

instance ParseTime t => Parsable (ParsableWrapper t)
  where parseParam p =
          let parseTimes t =
                msum $ map (flip (parseTime defaultTimeLocale) t)
                       -- The list of formats we accept. Full list in the
                       -- 'formatTime' documentation.
                         ["%F"
                         ,"%FT%T"]
          in  case parseTimes (unpack p) of
                Just pt -> Right $ ParsableWrap pt
                Nothing -> Left  $ pack "Ill-formatted time string"

paramE :: Parsable a => Text -> ActionE a
-- | 'paramE' looks up a parametre, raising 'errInvalidParam' if it fails to
-- parse the parametre and 'errMissingParam' if the parametre is not found.
paramE k = do
  val <- lookup k <$> params
  case val of
    Just v  -> either (\_ -> raiseE errInvalidParam) return $ parseParam v
    Nothing -> raiseE errMissingParam
  where raiseE e = raise $ e $ toStrict k

rescueMissing :: ActionE a -> ActionE (Maybe a)
-- | 'rescueMissing' takes an 'ActionE a' and returns 'Nothing' if
-- 'errMissingParam' is raised, or @'Just' a@ if there are no exceptions.
rescueMissing a = rescue (Just <$> a) $ \e -> case errorCode e of
  "MISSING_PARAM"   -> return Nothing
  _                 -> raise e

maybeParam :: Parsable a => Text -> ActionE (Maybe a)
-- | 'maybeParam' looks up a parametre and wraps it in a 'Maybe', returning
-- 'Nothing' if the parametre is not found.
maybeParam = rescueMissing . paramE

defaultParam :: Parsable a => a -> Text -> ActionE a
-- | 'defaultParam' looks up a parametre and returns a default value if the
-- parametre is not found.
defaultParam d p = fromMaybe d <$> maybeParam p
