{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Inits for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Spoc.Init where

import qualified Data.ByteString.Lazy as L
import qualified Data.Set as Z
import Network.Wai.Test
  (
  Session,
  )

import Tempuhs.Spoc.Assert
  (
  assertJSONOK,
  )
import Tempuhs.Spoc.Default
  (
  attributes,
  specifieds,
  )
import Tempuhs.Spoc.JSON
  (
  jsonKey,
  )
import Tempuhs.Spoc.Request
  (
  buildQueryL,
  post,
  )
import Tempuhs.Spoc.Type
  (
  AttributePair,
  Specified,
  )

initClock :: Session ()
-- | 'initClock' inserts a clock into an empty database and checks the
-- response.
initClock = post "/clocks" "name=TT" >>= assertJSONOK (jsonKey 1)

initTimespan :: Z.Set Specified -> [AttributePair] -> Session ()
-- | 'initTimespan' does 'initClock', then inserts a timespan specifying the
-- optionals that are members of the 'Z.Set' of 'Specified's (falling back to
-- default values for optionals not in the set), and checks the response.
-- 'TimespanAttribute's are inserted based on the given list of
-- 'AttributePair's.
initTimespan ss as =
  initClock >> post "/timespans" body >>= assertJSONOK (jsonKey 1)
  where body = L.intercalate "&" $
          ["clock=TT&beginMin=10.0"] ++
          [buildQueryL . filter (\(x,_) -> x `Z.member` ss) $
            [("beginMax", "15" :: String)
            ,("endMin",   "24")
            ,("endMax",   "42")]]    ++
          [buildQueryL as]

initTimespanAttrs :: [AttributePair] -> Session ()
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
    body = L.append "timespan=1&beginMin=0.0&" $ buildQueryL attributes

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

initUser :: Session ()
-- | 'initUser' inserts a user into an empty database and checks the
-- response.
initUser = post "/users" "name=Luser" >>= assertJSONOK (jsonKey 1)

initRole :: Session ()
-- | 'initRole' does 'initUser', and then inserts a role and checks the
-- response.
initRole = initUser >>
           post "/roles" "name=Rulle&namespace=1" >>= assertJSONOK (jsonKey 1)

initPermissionset :: Session ()
-- | 'initPermissionset' does 'initDefaultTimespan' and 'initRole', and then
-- inserts a permissionset, and checks the response.
initPermissionset = do
  initDefaultTimespan
  initRole
  post "/permissionsets" ("timespan=1&role=1&own=True&read=True" `L.append`
                         "&write=True") >>= assertJSONOK (jsonKey 1)