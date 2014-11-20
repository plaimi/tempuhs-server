{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server POST API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.POST where

import Control.Arrow
  (
  first,
  second,
  )
import Control.Monad
  (
  join,
  )
import Data.Maybe
  (
  fromMaybe,
  )
import Data.Functor
  (
  (<$>),
  )
import Database.Persist
  (
  Entity (Entity),
  Key,
  (=.),
  entityKey,
  getBy,
  delete,
  insert,
  repsert,
  update,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
  (
  params,
  )

import Plailude
import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  (=..),
  SqlPersistA,
  clockParam,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  maybeUnwrap,
  paramE,
  defaultParam,
  maybeParam,
  rescueMissing,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  jsonKey,
  jsonSuccess,
  )

postTimespan :: ConnectionPool -> ActionE ()
-- | 'postTimespan' inserts a new 'Timespan' into the database with the given
-- attributes. Or, if the ID of an existing 'Timespan' is given, updates that
-- instead.
postTimespan p = do
  timespan      <- maybeParam     "timespan"
  maybeParent   <- maybeParam     "parent"
  maybeBeginMin <- maybeParam     "beginMin"
  maybeBeginMax <- maybeParam     "beginMax"
  maybeEndMin   <- maybeParam     "endMin"
  maybeEndMax   <- maybeParam     "endMax"
  weight        <- defaultParam 1 "weight"
  rubbish       <- return Nothing
  -- If it ends with an '_', consider it an attribute. "&foo_=fu&bar_=baz".
  -- attrs is a list of key-value tuples.
  attrs         <- filter (L.isSuffixOf "_" . fst) <$> params

  -- "parent=n"  -> Just Just n
  -- "parent="   -> Just Nothing
  -- "parent"    -> Just Nothing
  -- Unspecified -> Nothing
  let parent = fmap mkKey . maybeUnwrap <$> maybeParent
      as     = map (both L.toStrict . first L.init) attrs

  runDatabase p $ do
    liftAE . jsonKey =<< case timespan of
      Just i  -> do
        let k = mkKey i
        clock <- liftAE . rescueMissing =<< erretreat (clockParam "clock")

        update k $ concat [TimespanParent   =.. parent
                          ,TimespanClock    =.. (entityKey <$> clock)
                          ,TimespanBeginMin =.. maybeBeginMin
                          ,TimespanBeginMax =.. maybeBeginMax
                          ,TimespanEndMin   =.. maybeEndMin
                          ,TimespanEndMax   =.. maybeEndMax
                          ]
        mapM_ (uncurry (updateAttribute k) . second Just) as
        return k
      Nothing -> do
        beginMin <- liftAE $ paramE "beginMin"
        -- If beginMax isn't specified, set it to beginMin + 1
        let beginMax         = fromMaybe (beginMin + (1 :: ProperTime))
                                         maybeBeginMax
        -- If endMin isn't specified, set it to endMax-1.
        -- If endMax isn't specified, set it to endMin+1.
        -- If neither are specified, set them to beginMax and beginMax+1.
            (endMin, endMax) =
              case (maybeEndMin, maybeEndMax) of
                (Nothing, Nothing) ->
                  (beginMin
                  ,fromMaybe (beginMin + (1 :: ProperTime)) maybeBeginMax)
                (Just a, Nothing)  -> (a, a + (1 :: ProperTime))
                (Nothing, Just b)  -> (b + (-1 :: ProperTime), b)
                (Just a, Just b)   -> (a, b)
        clock    <- clockParam "clock"
        tid      <- insert $ Timespan (join parent) (entityKey clock) beginMin
                                 beginMax endMin endMax weight rubbish
        mapM_ (insert . uncurry (TimespanAttribute tid)) as
        return tid

postAttribute :: ConnectionPool -> ActionE ()
-- | 'postAttribute' sets or removes a 'TimespanAttribute' based on a request.
postAttribute p = do
  timespan <- paramE     "timespan"
  key      <- paramE     "key"
  value    <- maybeParam "value"
  runDatabase p $ updateAttribute (mkKey timespan) key value

postClock :: ConnectionPool -> ActionE ()
-- | 'postClock' inserts a new 'Clock' into the database, or updates an
-- existing one, from a request.
postClock p = do
  clock <- maybeParam "clock"
  name  <- paramE     "name"
  runDatabase p $
    let c = Clock name
    in  liftAE . jsonKey =<< case clock of
      Just i  -> let k = mkKey i
                 in  repsert k c >> return k
      Nothing -> insert c

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

postUser :: ConnectionPool -> ActionE ()
-- | 'postUser' inserts a new 'User' into the database, or replaces an
-- existing one, from a request.
postUser p = do
  user <- maybeParam "user"
  name  <- paramE    "name"
  runDatabase p $
    let u = User name Nothing
    in  liftAE . jsonKey =<< case user of
      Just i  -> let k = mkKey i
                 in  repsert k u >> return k
      Nothing -> insert u

postRole :: ConnectionPool -> ActionE ()
-- | 'postRole' inserts a new 'Role' into the database, or updates an
-- existing one, from a request.
postRole p = do
  r <- maybeParam "role"
  runDatabase p $
    liftAE . jsonKey =<<
      case r of
        Just i -> do
          let k = mkKey i
          n  <- liftAE $ maybeParam "name"
          ns <- liftAE $ maybeParam "namespace"
          update (mkKey i) $ concat [RoleName      =.. n
                                    ,RoleNamespace =.. (mkKey <$> ns)
                                    ]
          return k
        Nothing -> do
          n  <- liftAE $ paramE "name"
          ns <- liftAE $ paramE "namespace"
          return =<< insert $ Role n (mkKey ns) Nothing
