{- |
Module      :  $Header$
Description :  Defaults for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Default where

import qualified Data.Set as Z

import Spoc.Type
  (
  AttributeKey,
  AttributeValue,
  Specified,
  )

specifieds :: Z.Set Specified
-- | A 'Z.Set' of all optional values that are specified.
specifieds = Z.fromList ["beginMax", "endMin", "endMax"]

attributes :: [(AttributeKey, AttributeValue)]
-- | A '[(AttributeKey, AttributeValue)]' of attributes for testing.
attributes = [("foo", "fu"), ("bar", "baz")]
