{- |
Module      :  $Header$
Description :  Types for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Type where

type AttributeKey   = String
type AttributeValue = String
type Specified      = String

buildAttribute :: [(AttributeKey, AttributeValue)] -> String
buildAttribute ((k,v):xs) = '&' : k ++ "_=" ++ v ++ buildAttribute xs
buildAttribute []         = []
