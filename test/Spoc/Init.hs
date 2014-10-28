{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Inits for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Init where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Set as Z
import Network.Wai.Test
  (
  Session,
  )

import Spoc.Assert
  (
  assertJSONOK,
  )
import Spoc.Default
  (
  attributes,
  specifieds,
  )
import Spoc.JSON
  (
  jsonKey,
  )
import Spoc.Request
  (
  post,
  )
import Spoc.Type
  (
  AttributeKey,
  AttributeValue,
  Specified,
  buildAttribute,
  )

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 1)

initTimespan :: Z.Set Specified -> [(AttributeKey, AttributeValue)]
             -> Session ()
-- | 'initTimespan' does 'initClock', then inserts a timespan specifying the
-- optionals that are members of the 'Z.Set' of 'Specified's (falling back to
-- default values for optionals not in the set), and checks the response.
-- 'TimespanAttribute's are inserted based on the given list of
-- 'AttributeKey'-'AttributeValue'-pairs.
initTimespan ss as =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = L8.pack . concat $
          ["clock=TT&beginMin=10.0"]                  ++
          ["&beginMax=15" | "beginMax" `Z.member` ss] ++
          ["&endMin=24"   | "endMin"   `Z.member` ss] ++
          ["&endMax=42"   | "endMax"   `Z.member` ss] ++
          [buildAttribute as]

initTimespanAttrs :: [(AttributeKey, AttributeValue)] -> Session ()
-- | 'initTimespanAttrs' does 'initTimespan' without 'Specified's.
initTimespanAttrs = initTimespan Z.empty

initTimespanSpecs :: Z.Set Specified -> Session ()
-- | 'initTimespanSpecs' does 'initTimespan' without 'TimespanAttribute's.
initTimespanSpecs = flip initTimespan []

initDefaultTimespan :: Session ()
-- | 'initDefaultTimespan' inserts a default 'Timespan'.
initDefaultTimespan = initTimespan specifieds attributes

initModTimespan :: Session ()
-- | 'initModTimespan' posts a 'Timespan', then modifies the existing timespan
-- and checks the response.
initModTimespan =
  initDefaultTimespan >>
    post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where
    body =
      L8.pack $ "timespan=1&clock=TT&beginMin=10.0" ++
                buildAttribute attributes

initSubTimespan :: Session ()
-- | 'initSubTimespan' does 'initDefaultTimespan', then inserts another
-- timespan with the first timespan as parent and checks the response.
initSubTimespan =
  initDefaultTimespan >>
    post "/timespans" body >>= assertJSONOK (jsonKey 2)
  where
    body =
      "parent=1&clock=TT&beginMin=-10.0"

initAttribute :: Session ()
-- | 'initAttribute' does 'initTimespanSpecs', then inserts a timespan
-- attribute and checks the response.
initAttribute =
  initTimespanSpecs specifieds >>
    post "/attributes" body >>= assertJSONOK (jsonKey 1)
  where body = "timespan=1&key=title&value=test"
