{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Timespan API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Timespan where

import Control.Arrow
  (
  first,
  second,
  )
import Control.Monad
  (
  join,
  void,
  )
import qualified Database.Esqueleto as E
import Database.Esqueleto
  (
  (^.),
  (&&.),
  (<=.),
  (>=.),
  asc,
  from,
  isNothing,
  orderBy,
  val,
  where_,
  )
import Data.Foldable
  (
  toList,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  fromMaybe,
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
  json,
  params,
  )

import Plailude
import Tempuhs.Chronology hiding (second)
import Tempuhs.Server.Database
  (
  SqlPersistA,
  (=..),
  attributeSearch,
  clockParam,
  getAttrs,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.DELETE
  (
  nowow,
  owow,
  )
import Tempuhs.Server.Param
  (
  ParsableWrapper (parsableUnwrap),
  defaultParam,
  maybeParam,
  maybeUnwrap,
  paramE,
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
postTimespan pool = do
  t             <- maybeParam     "timespan"
  maybeParent   <- maybeParam     "parent"
  maybeBeginMin <- maybeParam     "beginMin"
  maybeBeginMax <- maybeParam     "beginMax"
  maybeEndMin   <- maybeParam     "endMin"
  maybeEndMax   <- maybeParam     "endMax"
  w             <- defaultParam 1 "weight"
  r             <- return Nothing
  -- If it ends with an '_', consider it an attribute. "&foo_=fu&bar_=baz".
  -- attrs is a list of key-value tuples.
  attrs         <- filter (L.isSuffixOf "_" . fst) <$> params

  -- "parent=n"  -> Just Just n
  -- "parent="   -> Just Nothing
  -- "parent"    -> Just Nothing
  -- Unspecified -> Nothing
  let p  = fmap mkKey . maybeUnwrap <$> maybeParent
      as = map (both L.toStrict . first L.init) attrs

  runDatabase pool $ do
    liftAE . jsonKey =<< case t of
      Just i  -> do
        let k = mkKey i
        c <- liftAE . rescueMissing =<< erretreat (clockParam "clock")

        update k $ concat [TimespanParent   =.. p
                          ,TimespanClock    =.. (entityKey <$> c)
                          ,TimespanBeginMin =.. maybeBeginMin
                          ,TimespanBeginMax =.. maybeBeginMax
                          ,TimespanEndMin   =.. maybeEndMin
                          ,TimespanEndMax   =.. maybeEndMax
                          ]
        mapM_ (uncurry (updateAttribute k) . second Just) as
        return k
      Nothing -> do
        bMin <- liftAE $ paramE "beginMin"
        -- If beginMax isn't specified, set it to beginMin + 1
        let bMax         = fromMaybe (bMin + (1 :: ProperTime)) maybeBeginMax
        -- If endMin isn't specified, set it to endMax-1.
        -- If endMax isn't specified, set it to endMin+1.
        -- If neither are specified, set them to beginMax and beginMax+1.
            (eMin, eMax) =
              case (maybeEndMin, maybeEndMax) of
                (Nothing, Nothing) ->
                  (bMin, fromMaybe (bMin + (1 :: ProperTime)) maybeBeginMax)
                (Just a, Nothing)  -> (a, a + (1 :: ProperTime))
                (Nothing, Just b)  -> (b + (-1 :: ProperTime), b)
                (Just a, Just b)   -> (a, b)
        c    <- clockParam "clock"
        tid  <- insert $ Timespan (join p) (entityKey c) bMin bMax eMin eMax
                                  w r
        mapM_ (insert . uncurry (TimespanAttribute tid)) as
        return tid

timespans :: ConnectionPool -> ActionE ()
-- | 'timespans' serves a basic request for a list of 'Timespan's with their
-- associated 'TimespanAttribute's.
timespans pool = do
  p     <- maybeParam "parent"
  b     <- maybeParam "begin"
  e     <- maybeParam "end"
  r     <- maybeParam "rubbish"
  joins <- attributeSearch
  let filters t =
        (cmpMaybe (E.==.) (t ^. TimespanParent)  $ mkKey          <$> p) :
        (cmpMaybe (>=.)   (t ^. TimespanRubbish) $ parsableUnwrap <$> r) :
        [t ^. TimespanBeginMin <=. val x | x <- toList e]                ++
        [t ^. TimespanEndMax   >=. val x | x <- toList b]
  runDatabase pool $ do
    c <- liftAE . rescueMissing =<< erretreat (clockParam "c")
    let clockFilter t =
          [t ^. TimespanClock E.==. val (entityKey d) | d <- toList c]
    list <- E.select $
      joinList joins $
        E.from $ \t -> do
          E.where_ $ foldl (&&.) (val True) (clockFilter t ++ filters t)
          orderBy [asc $ t ^. TimespanId]
          return t
    liftAE . json =<< mapM (\a -> (,) a <$> getAttrs a) list
  where joinList (e:es) t =
          joinList es t >>= \t' -> from $ \b -> where_ (e t' b) >> return t'
        joinList []     t = t
        cmpMaybe f a b    = case b of
                              Just _ -> f a $ val b
                              _      -> isNothing a

deleteTimespan :: ConnectionPool -> ActionE ()
-- | 'deleteTimespan' updates the rubbish field of an existing 'Timespan'.
deleteTimespan = nowow "timespan" TimespanRubbish

postAttribute :: ConnectionPool -> ActionE ()
-- | 'postAttribute' sets or removes a 'TimespanAttribute' based on a request.
postAttribute p = do
  t     <- paramE     "timespan"
  key   <- paramE     "key"
  value <- maybeParam "value"
  runDatabase p $ updateAttribute (mkKey t) key value

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

unsafeDeleteTimespan :: ConnectionPool -> ActionE ()
-- | 'unsafeDeleteClock' hard-deletes a 'Timespan' from the database.
unsafeDeleteTimespan p =
  void $ (owow "timespan" p :: ActionE (Maybe (Key Timespan)))
