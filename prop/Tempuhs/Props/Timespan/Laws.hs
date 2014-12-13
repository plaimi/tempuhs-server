{- |
Module      :  $Header$
Description :  Property tests for timespans.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Props.Timespan.Laws where

--FlexLaws
isFlexLaw :: String
isFlexLaw = "A timespan with a clock with a name of \"<~>\" is a flexible timespan and subject to the laws of this section."

beginMinLaw :: String
beginMinLaw = "A flexible timespan's beginMin is equal to the smallest beginMin of all of its immediate descendants' beginMin."

endMaxLaw :: String
endMaxLaw = "A flexible timespan's endMax is equal to the biggest endMax of all of its immediate descendants' endMax."

-- ParentLaws
notDescParentLaw :: String
notDescParentLaw = "A timespan may not have a descendant as a parent."

notSelfParentLaw :: String
notSelfParentLaw = "A timespan may not have itself as a parent."
