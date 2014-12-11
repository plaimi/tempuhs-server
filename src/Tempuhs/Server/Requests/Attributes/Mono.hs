{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Module      :  $Header$
Description :  Attributes functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Attributes.Mono where

import Control.Monad.Trans.Reader
  (
  ReaderT,
  )
import Control.Monad.Trans.Resource.Internal
  (
  ResourceT,
  )
import qualified Database.Esqueleto as E
import Data.Text
  (
  Text,
  )
import Database.Persist
  (
  Entity,
  Key (Key),
  )
import Database.Persist.Sql
  (
  SqlBackend,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Requests.Attributes.Poly
import Tempuhs.Server.Spock
  (
  ActionE,
  )


getUserAttrs :: Entity User
             -> ReaderT SqlBackend (ResourceT ActionE) [Entity UserAttribute]
-- | 'getUserAttrs' is a wrapper around 'getAttrs' for 'UserAttribute's.
getUserAttrs a = getAttrs a UserAttributeUser UserAttributeId

updateUserAttributes :: Key User -> [(Text, Text)]
                         -> ReaderT SqlBackend (ResourceT ActionE) ()
-- | 'updateUserAttribute' is a wrapper around 'updateAttributes' for
-- 'UserAttribute's.
updateUserAttributes i = updateAttributes i UniqueUserAttribute
                                                UserAttributeValue
                                                UserAttribute


updateTimespanAttributes :: Key Timespan -> [(Text, Text)]
                         -> ReaderT E.SqlBackend (ResourceT ActionE) ()
-- | 'updateTimespanAttributes' is a wrapper around 'updateAttributes' for
-- 'TimespanAttribute's.
updateTimespanAttributes i = updateAttributes i UniqueTimespanAttribute
                                                TimespanAttributeValue
                                                TimespanAttribute

getTimespanAttrs :: E.Entity Timespan
                 -> ReaderT E.SqlBackend (ResourceT ActionE)
                      [E.Entity TimespanAttribute]
-- | 'getTimespanAttrs' is a wrapper around 'getAttrs' for
-- 'TimespanAttribute's.
getTimespanAttrs a = getAttrs a TimespanAttributeTimespan TimespanAttributeId
