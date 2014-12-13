{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Pure law enforcers.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Laws.Props where

import Plailude
import Tempuhs.Chronology

isFlexProp :: Clock -> Bool
-- | 'beginMinProp' enforces the FlexiTime clock name law
-- (see /docs/LAWS.txt).
isFlexProp c = (clockName c == "<~>")

beginMinProp :: [Timespan] -> ProperTime
-- | 'beginMinProp' enforces the FlexiTime beginMin law (see /docs/LAWS.txt).
beginMinProp = minimum . map timespanBeginMin

endMaxProp :: [Timespan] -> ProperTime
-- | 'beginMinProp' enforces the FlexiTime endMax law (see /docs/LAWS.txt).
endMaxProp = maximum . map timespanEndMax

parentCycleProp :: Eq a => a -> [a] -> Bool
-- | 'parentCycleProp' enforces the parent cycle laws (see /docs/LAWS.txt).
parentCycleProp = not .: elem
