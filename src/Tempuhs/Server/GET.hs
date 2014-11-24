{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  The tempuhs server GET API.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.GET where

import Data.Foldable
  (
  toList,
  )
import Data.Functor
  (
  (<$>),
  )
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
import qualified Database.Esqueleto as E
import Database.Persist
  (
  SelectOpt (Asc),
  (==.),
  entityKey,
  selectList,
  )
import Database.Persist.Sql
  (
  ConnectionPool,
  )
import Web.Scotty.Trans
  (
  json,
  )

import Plailude
import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  attributeSearch,
  clockParam,
  getAttrs,
  liftAE,
  mkKey,
  runDatabase,
  )
import Tempuhs.Server.Param
  (
  ParsableWrapper (parsableUnwrap),
  maybeParam,
  rescueMissing,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  )

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

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [ClockName ==. cn       | cn <- toList n] ++
                [ClockId   ==. mkKey ci | ci <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc ClockId]

users :: ConnectionPool -> ActionE ()
-- | 'users' serves a request for a list of 'User's.
users p = do
  n <- maybeParam "name"
  i <- maybeParam "id"
  let filters = [UserName ==. un       | un <- toList n] ++
                [UserId   ==. mkKey ui | ui <- toList i]
  runDatabase p $ liftAE . json =<< selectList filters [Asc UserId]

roles :: ConnectionPool -> ActionE ()
-- | 'roles' serves a request for a list of 'Role's.
roles p = do
  n <- maybeParam "name"
  s <- maybeParam "namespace"
  r <- maybeParam "id"
  let filters = [RoleName      ==. rn       | rn <- toList n] ++
                [RoleNamespace ==. mkKey rs | rs <- toList s] ++
                [RoleId        ==. mkKey ri | ri <- toList r]
  runDatabase p $ liftAE . json =<< selectList filters [Asc RoleId]
