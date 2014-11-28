{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Defaults for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Spoc.Default where

import qualified Data.Set as Z

import Tempuhs.Spoc.Type
  (
  AttributePair,
  Specified,
  )

specifieds :: Z.Set Specified
-- | A 'Z.Set' of all optional values that are specified.
specifieds = Z.fromList ["beginMax", "endMin", "endMax"]

attributes :: [AttributePair]
-- | A '[AttributePair]' of attributes for testing.
attributes = [("foo", "fu"), ("bar", "baz")]
