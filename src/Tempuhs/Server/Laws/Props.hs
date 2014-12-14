{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Pure law enforcers. See /docs/LAWS.txt.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Laws.Props where

import Plailude
import Tempuhs.Chronology

isFlexProp :: Clock -> Bool
-- | 'isFlexProp' enforces the FlexiTime clock name law.
isFlexProp c = (clockName c == "<~>")

beginMinProp :: [Timespan] -> ProperTime
-- | 'beginMinProp' enforces the FlexiTime beginMin law.
beginMinProp = minimum . map timespanBeginMin

endMaxProp :: [Timespan] -> ProperTime
-- | 'endMaxProp' enforces the FlexiTime endMax law.
endMaxProp = maximum . map timespanEndMax

parentCycleProp :: Eq a => a -> [a] -> Bool
-- | 'parentCycleProp' enforces the parent cycle laws.
parentCycleProp = not .: elem
