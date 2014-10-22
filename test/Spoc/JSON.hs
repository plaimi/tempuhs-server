{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  JSON stuff for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.JSON where

import Data.Aeson
  (
  ToJSON,
  Value,
  (.=),
  encode,
  object,
  )
import qualified Data.ByteString.Lazy.Char8 as L8

jsonSuccess :: Value
-- | 'jsonSuccess' is the result of a successful operation without any data to
-- return.
jsonSuccess = object []

jsonKey :: Integer -> Value
-- | 'jsonKey' is the json representation of a database key.
jsonKey k = object ["id" .= k]

showJSON :: ToJSON a => a -> String
-- | 'showJSON' returns the JSON representation of the given value as a
-- 'String'.
showJSON = L8.unpack . encode
