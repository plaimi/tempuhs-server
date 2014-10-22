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
  )

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 1)

initTimespan :: Z.Set Specified -> Session ()
-- | 'initTimeSpan' does 'initClock', then inserts a timespan specifying the
-- optionals that are members of the 'Z.Set' of 'Specified's (falling back to
-- default values for optionals not in the set), and checks the response.
initTimespan ss =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = L8.pack . concat $
          ["clock=TT&beginMin=10.0"]                  ++
          ["&beginMax=15" | "beginMax" `Z.member` ss] ++
          ["&endMin=24"   | "endMin"   `Z.member` ss] ++
          ["&endMax=42"   | "endMax"   `Z.member` ss]

initModTimespan :: Session ()
-- | 'initModTimespan' does 'initTimespan', then modifies the existing
-- timespan and checks the response.
initModTimespan =
  initTimespan specifieds >>
    post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = "timespan=1&clock=TT&beginMin=0&beginMax=1&endMin=9&endMax=10"

initSubTimespan :: Session ()
-- | 'initSubTimespan' does 'initTimespan', then inserts another timespan with
-- the first timespan as parent and checks the response.
initSubTimespan =
  initTimespan specifieds >>
    post "/timespans" body >>= assertJSONOK (jsonKey 2)
  where
    body =
      "parent=1&clock=TT&beginMin=-9.0&beginMax=-8.0&endMin=8.0&endMax=9.0"

initTimespanWithAttrs :: [(AttributeKey, AttributeValue)] -> Session ()
-- | 'initTimespanWithAttrs' posts a 'Timespan' with the given list of
-- 'AttributeKey's and 'AttributeValue's.
initTimespanWithAttrs as =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where
    body             = L8.pack $ "clock=TT&beginMin=10.0" ++ build as
    build ((k,v):xs) = '&' : k ++ "_=" ++ v ++ build xs
    build []         = []

initAttribute :: Session ()
-- | 'initAttribute' does 'initTimespan', then inserts a timespan attribute
-- and checks the response.
initAttribute =
  initTimespan specifieds >>
    post "/attributes" body >>= assertJSONOK (jsonKey 1)
  where body = "timespan=1&key=title&value=test"
