{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  $Header$
Description :  Types for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Type where

import Data.String
  (
  IsString,
  )
import Data.Stringable
  (
  Stringable,
  )
import Network.HTTP.Types.QueryLike
  (
  QueryKeyLike,
  QueryValueLike,
  toQueryKey,
  )

type Specified      = String
type AttributePair  = (AttributeKey, AttributeValue)

newtype AttributeKey   = MkAttrKey String
                         deriving (IsString, Stringable)
newtype AttributeValue = MkAttrVal String
                         deriving (IsString, QueryValueLike, Stringable)

instance QueryKeyLike AttributeKey where
  toQueryKey (MkAttrKey s) = toQueryKey $ s ++ "_"
