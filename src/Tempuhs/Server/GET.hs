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
timespans p = do
  parent  <- maybeParam "parent"
  begin   <- maybeParam "begin"
  end     <- maybeParam "end"
  rubbish <- maybeParam "rubbish"
  joins   <- attributeSearch
  let filters t =
        (cmpMaybe (E.==.) (t ^. TimespanParent) $ mkKey <$> parent) :
        (cmpMaybe (>=.)   (t ^. TimespanRubbish) $
                  parsableUnwrap <$> rubbish)                       :
        [t ^. TimespanBeginMin <=. val x | x <- toList end]         ++
        [t ^. TimespanEndMax   >=. val x | x <- toList begin]
  runDatabase p $ do
    clock <- liftAE . rescueMissing =<< erretreat (clockParam "clock")
    let clockFilter t =
          [t ^. TimespanClock E.==. val (entityKey c) | c <- toList clock]
    list <- E.select $
      joinList joins $
        E.from $ \t -> do
          E.where_ $ foldl (&&.) (val True) (clockFilter t ++ filters t)
          orderBy [asc $ t ^. TimespanId]
          return t
    liftAE . json =<< mapM (\e -> (,) e <$> getAttrs e) list
  where joinList (e:es) t =
          joinList es t >>= \t' -> from $ \b -> where_ (e t' b) >> return t'
        joinList []     t = t
        cmpMaybe f a b    = case b of
                              Just _ -> f a $ val b
                              _      -> isNothing a

clocks :: ConnectionPool -> ActionE ()
-- | 'clocks' serves a request for a list of 'Clock's.
clocks p = do
  name <- maybeParam "name"
  cid  <- maybeParam "id"
  let filters = [ClockName ==. x | x <- toList name] ++
                  [ClockId ==. mkKey x | x <- toList cid]
  runDatabase p $ liftAE . json =<< selectList filters [Asc ClockId]
