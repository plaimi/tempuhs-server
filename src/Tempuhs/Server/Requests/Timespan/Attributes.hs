{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Timespan attributes functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Timespan.Attributes where

import Control.Arrow
  (
  first,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  Entity (Entity),
  Key (Key),
  (=.),
  getBy,
  delete,
  insert,
  update,
  )
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
  (
  params,
  )

import Plailude
import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  SqlPersistA,
  liftAE,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  jsonSuccess,
  )

updateAttribute :: Key Timespan -> T.Text -> Maybe T.Text -> SqlPersistA ()
-- | 'updateAttribute' updates the 'TimespanAttribute' of a 'Timespan'. It
-- takes a @'Key' 'Timespan'@, which is the ID of the 'Timespan' the attribute
-- is related to, a key, and a 'Maybe' value. If the key already exists in the
-- database, it is updated. If not, it is inserted. If the value is 'Nothing',
-- the key is deleted if it exists. If the 'Timespan' does not exist, nothing
-- happens.
updateAttribute tid k v = do
    ma <- getBy $ UniqueTimespanAttribute tid k
    case v of
      Nothing -> deleteAttribute ma
      Just w  -> liftAE . jsonKey =<<
        case ma of
          Just (Entity aid _) -> do
            update aid [TimespanAttributeValue =. w]
            return aid
          Nothing -> insert $ TimespanAttribute tid k w

deleteAttribute :: Maybe (Entity TimespanAttribute) -> SqlPersistA ()
-- | 'deleteAttribute' takes a 'Maybe' attribute and deletes it if it's 'Just'
-- an attribute. If it's 'Nothing' this means the attribute doesn't exist in
-- the database, so nothing happens.
deleteAttribute a = do
  case a of
    Just (Entity aid _) -> delete aid
    Nothing             -> return ()
  liftAE jsonSuccess

attributesParams :: ActionE [(T.Text, T.Text)]
-- | 'attributeParams' gets all attributes form the '_' parametres. If a
-- parametre ends with an '_', it is considered an attribute.
-- "&foo_=fu&bar_=baz". attrs is a list of key-value tuples.
attributesParams = filter (L.isSuffixOf "_" . fst) <$> params
               >>= return . map (both L.toStrict . first L.init)
