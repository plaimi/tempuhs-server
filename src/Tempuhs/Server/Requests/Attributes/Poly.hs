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
-} module Tempuhs.Server.Requests.Attributes.Poly where

import Control.Arrow
  (
  first,
  second,
  )
import Control.Monad
  (
  liftM,
  )
import Control.Monad.IO.Class
  (
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

import Plailude
import Tempuhs.Server.Database
  (
  liftAE,
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
  jsonKey,
  jsonSuccess,
  )

patchAttribute :: forall r g f u. (Parsable g, Parsable f, PersistEntity u
                                  ,PersistEntity r, PersistField f
                                  ,PersistEntityBackend u ~ SqlBackend)
               => L.Text
               -> (Key r -> g -> Unique u)
               -> EntityField u f
               -> (Key r -> g -> f -> u)
               -> ConnectionPool
               -> ActionE ()
-- | 'patchAttribute' looks up the passed in ID parametre, inserts or updates
-- or deletes the passed in key with the passed in value if applicable. It
-- also needs the getter-field for the attribute, along with the value field
-- to update and the row to insert
patchAttribute i g f r p = do
  j <- paramE     i
  k <- paramE     "key"
  v <- maybeParam "value"
  runDatabase p $ let kk = mkKey j in updateAttribute kk k v g f r

updateAttribute :: forall v g f k. (PersistEntity g, PersistField v
                                   ,PersistEntityBackend g ~ SqlBackend)
                => f
                -> k
                -> Maybe v
                -> (f -> k -> Unique g)
                -> EntityField g v
                -> (f -> k -> v -> g)
                -> ReaderT SqlBackend (ResourceT ActionE) ()
-- | 'updateAttribute' updates the passed in attribute. It takes a @'Key' a@,
-- which is the ID of the resource the attribute is related to, a key, and a
-- 'Maybe' value. If the key already exists in the database, it is updated. If
-- not, it is inserted. If the value is 'Nothing', the key is deleted if it
-- exists. If the resource does not exist, nothing happens.
updateAttribute i k v g f r = do
    a <- getBy $ g i k
    case v of
      Nothing -> deleteAttribute a
      Just w  -> liftAE . jsonKey =<<
        case a of
          Just (Entity aid _) -> update aid [f =. w] >> return aid
          Nothing -> insert $ r i k w

deleteAttribute :: forall a. PersistEntityBackend a ~ SqlBackend
                => Maybe (Entity a)
                -> ReaderT SqlBackend (ResourceT ActionE) ()
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
attributesParams = liftM (map (both L.toStrict . first L.init))
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

insertAttributes :: forall t (m :: * -> *) ak av k.
                          (MonadIO m, PersistEntity t
                          ,E.PersistStore (PersistEntityBackend t))
                 => (k -> ak -> av -> t)
                 -> [(ak, av)]
                 -> k
                 -> ReaderT (PersistEntityBackend t) m ()
-- | 'insertAttributes' inserts the given attributes (as) of the given type
-- (t) with the given resource key (k).
insertAttributes t as k = mapM_ (insert . uncurry (t k)) as

updateAttributes :: forall v g f k.
                           (PersistEntity g, PersistField v
                           ,PersistEntityBackend g ~ SqlBackend)
                 => f
                 -> (f -> k -> Unique g)
                 -> EntityField g v
                 -> (f -> k -> v -> g)
                 -> [(k, v)]
                 -> ReaderT SqlBackend (ResourceT ActionE) ()
-- | 'updateAttributes' updates the given attributes with the given resource
-- ID.
updateAttributes i g r f =
  mapM_ (uncurry (\k v -> updateAttribute i k v g r f) . second Just)
