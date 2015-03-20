{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Module      :  $Header$
Description :  Attributes functions.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Attributes.Poly where

import Control.Monad
  (
  liftM,
  void,
  )
import Control.Monad.IO.Class
  (
  liftIO,
  MonadIO,
  )
import Control.Monad.Trans.Reader
  (
  ReaderT,
  )
import Control.Monad.Trans.Resource.Internal
  (
  ResourceT,
  )
import Database.Esqueleto
  (
  Value,
  (^.),
  (&&.),
  like,
  val,
  )
import qualified Database.Esqueleto as E
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  fromMaybe,
  )
import Data.Monoid
  (
  Monoid,
  mempty,
  )
import Data.String
  (
  IsString,
  )
import Data.Time.Clock
  (
  getCurrentTime,
  UTCTime,
  )
import Database.Persist
  (
  Entity (Entity),
  SelectOpt (Asc),
  Key (Key),
  (=.),
  (==.),
  entityKey,
  getBy,
  delete,
  insert,
  selectList,
  update,
  )
import Database.Persist.Class
  (
  EntityField,
  PersistEntity,
  PersistEntityBackend,
  PersistField,
  PersistQuery,
  Unique,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  SqlBackend,
  )
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import Web.Scotty
  (
  Parsable,
  )
import Web.Scotty.Internal.Types
  (
  ActionT,
  ScottyError,
  )
import Web.Scotty.Trans
  (
  params,
  )

import Tempuhs.Server.Database
  (
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  maybeParam,
  paramE,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  )

patchAttribute :: forall g k v u r w s.
                  (Parsable v, Parsable k, PersistEntity s, PersistEntity r
                  ,PersistEntity g, PersistField v, IsString v
                  ,PersistEntityBackend r ~ SqlBackend
                  ,PersistEntityBackend g ~ SqlBackend)
               => L.Text
               -> (Key s -> k -> Unique g)
               -> EntityField g v
               -> (Key s -> k -> v -> Maybe u -> r)
               -> EntityField g (Maybe UTCTime)
               -> (g -> Maybe w)
               -> ConnectionPool
               -> ActionE ()
-- | 'patchAttribute' looks up the passed in ID parametre, inserts or updates
-- or deletes the passed in key with the passed in value if applicable. It
-- also needs the getter-field for the attribute, along with the value field
-- to update and the row to insert
patchAttribute i g f r rf rg p = do
  j <- paramE     i
  k <- paramE     "key"
  v <- maybeParam "value"
  runDatabase p $ updateAttribute (mkKey j) k v g f r rf rg

updateAttribute :: forall g f k v u r w.
                   (PersistEntity r, PersistEntity g, PersistField v
                   ,IsString v, PersistEntityBackend r ~ SqlBackend
                   ,PersistEntityBackend g ~ SqlBackend)
                => f
                -> k
                -> Maybe v
                -> (f -> k -> Unique g)
                -> EntityField g v
                -> (f -> k -> v -> Maybe u -> r)
                -> EntityField g (Maybe UTCTime)
                -> (g -> Maybe w)
                -> ReaderT SqlBackend (ResourceT ActionE) ()
-- | 'updateAttribute' updates the passed in attribute. It takes a @'Key' a@,
-- which is the ID of the resource the attribute is related to, a key, and a
-- 'Maybe' value. If the key already exists in the database, it is updated. If
-- not, it is inserted. If the value is 'Nothing', the key is deleted if it
-- exists. If the resource does not exist, nothing happens.
updateAttribute i k v g f r rf rg = do
    a <- getBy $ g i k
    case (a, v) of
      (Nothing, Nothing)           -> return ()
      (Nothing, Just w)            -> void $ insert (r i k w  Nothing)
      (Just (Entity b c), Nothing) ->
        case rg c of
          Just _  -> delete b
          Nothing -> do
            now <- liftIO getCurrentTime
            update b [rf =. Just now]
      (Just (Entity b _), Just w)  -> update b [f =. w]

attributesParams :: ActionE [(T.Text, Maybe T.Text)]
-- | 'attributeParams' gets all attributes from the '_' parametres. If a
-- parametre ends with an '_', it is considered an attribute.
-- "&foo_=fu&bar_=baz". Valid attributes come in a list of key—'Maybe' value
-- tuples.
attributesParams =
  liftM (map
          (\(k, v) -> (L.toStrict (L.init k)
                      ,if L.null v then Nothing else Just (L.toStrict v))))
           $ filter (L.isSuffixOf "_" . fst) <$> params

attributeSearch :: forall e (m :: * -> *) (query :: * -> *)
                          backend (expr :: * -> *) typ v w.
                          (E.Esqueleto query expr backend, ScottyError e
                          ,PersistEntity w, PersistEntity v
                          ,PersistField typ, Monad m)
                 => EntityField v typ
                 -> EntityField w typ
                 -> EntityField v T.Text
                 -> EntityField v T.Text
                 -> ActionT e m [expr (Entity w)
                              -> expr (Entity v)
                              -> expr (Value Bool)]
-- | 'attributeSearch' uses the list of request parametres to construct a list
-- of functions that can be used in an 'Esqueleto' query to filter out
-- resources based on their attributes. It takes an attribute key field, an
-- attribute value field and an associated resource ID field and ID.
attributeSearch aa ai ak av = do
  ps <- params
  return $ do
    (p, v) <- ps
    let (n_, o) = T.breakOnEnd "_" $ L.toStrict p
    (#) <- case o of
             ""     -> [(E.==.)]
             "like" -> [like]
             _      -> []
    case T.stripSuffix "_" n_ of
      Just n -> [\t a ->
                   a ^. ak E.==. val n &&.
                   a ^. av # val (L.toStrict v) &&.
                   a ^. aa E.==. t ^. ai]
      _      -> []

getAttrs :: forall v (m :: * -> *) r t.
                   (MonadIO m, PersistEntity v, PersistField (Key r)
                   ,PersistQuery (PersistEntityBackend v))
         => Entity r -> EntityField v (Key r) -> EntityField v t
         -> ReaderT (PersistEntityBackend v) m [Entity v]
-- | 'getAttrs' returns a list of all attributes for a given associated
-- resource ID and field, and the attribute ID field.
getAttrs a f i = selectList [f ==. entityKey a] [Asc i]

insertAttributes :: forall ak av r (m :: * -> *) k u.
                    (MonadIO m, PersistEntity r, Monoid av
                    ,E.PersistStore (PersistEntityBackend r))
                 => (k -> ak -> av -> Maybe u -> r)
                 -> [(ak, Maybe av)]
                 -> k
                 -> ReaderT (PersistEntityBackend r) m ()
-- | 'insertAttributes' inserts the given attributes (as) of the given type
-- (t) with the given resource key (k).
insertAttributes t kv f = mapM_ (\(k, v)
                       -> insert (t f k (fromMaybe mempty v) Nothing)) kv

updateAttributes :: forall g f k u r w v.
                    (PersistEntity r, PersistEntity g
                    ,PersistField v, IsString v
                    ,PersistEntityBackend r ~ SqlBackend
                    ,PersistEntityBackend g ~ SqlBackend)
                 => f
                 -> (f -> k -> Unique g)
                 -> EntityField g v
                 -> (f -> k -> v -> Maybe u -> r)
                 -> EntityField g (Maybe UTCTime)
                 -> (g -> Maybe w)
                 -> [(k, Maybe v)]
                 -> ReaderT SqlBackend (ResourceT ActionE) ()
-- | 'updateAttributes' updates the given attributes with the given resource
-- ID.
updateAttributes i g f r rf rg =
  mapM_ (uncurry (\k v -> updateAttribute i k v g f r rf rg))
