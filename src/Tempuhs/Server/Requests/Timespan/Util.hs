{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- |
Module      :  $Header$
Description :  Timespan utility functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Requests.Timespan.Util where

import Control.Monad
  (
  forM,
  join,
  )
import Control.Monad.IO.Class
  (
  MonadIO,
  )
import Control.Monad.Trans.Reader
  (
  ReaderT,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  fromMaybe,
  )
import qualified Database.Esqueleto as E
import Database.Esqueleto
  (
  (^.),
  (<=.),
  (>=.),
  asc,
  orderBy,
  val,
  )
import Data.Foldable
  (
  Foldable,
  toList,
  )
import Database.Persist
  (
  Entity,
  Key (Key),
  entityKey,
  )
import Data.Time.Clock
  (
  UTCTime,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Param
  (
  ParsableWrapper (parsableUnwrap),
  maybeParam,
  paramE,
  )
import Tempuhs.Server.Database
  (
  cmpMaybe,
  mkKey,
  )
import Tempuhs.Server.Spock
  (
  ActionE,
  )

filters :: forall (query :: * -> *) backend (expr :: * -> *)
                  (t :: * -> *) (t1 :: * -> *).
                  (E.Esqueleto query expr backend, Foldable t1, Foldable t)
        => expr (Entity Timespan)
        -> Maybe Integer
        -> Maybe (ParsableWrapper UTCTime)
        -> t ProperTime
        -> t1 ProperTime -> [expr (E.Value Bool)]
-- | 'filters' makes a list of 'Esqueleto' expressions for filtering on
-- 'TimespanParent', 'TimespanRubbish', 'TimespanBeginMin' and
-- 'TimespanEndMax'.
filters t p r e b =
      (cmpMaybe (E.==.) (t ^. TimespanParent)  $ mkKey          <$> p) :
      (cmpMaybe (>=.)   (t ^. TimespanRubbish) $ parsableUnwrap <$> r) :
      [t ^. TimespanBeginMin <=. val x | x <- toList e]               ++
      [t ^. TimespanEndMax   >=. val x | x <- toList b]

clockFilter :: forall (t :: * -> *) (query :: * -> *)
                      (expr :: * -> *) backend.
                      (E.Esqueleto query expr backend, Foldable t)
            => expr (Entity Timespan)
            -> t (Entity Clock) -> [expr (E.Value Bool)]
--  | 'clockFilter' makes a list of 'Esqueleto' expressions for filtering on
--  'TimespanClock'
clockFilter t c = [t ^. TimespanClock E.==. val (entityKey d) | d <- toList c]

idFilter :: forall (t :: * -> *) (query :: * -> *) (expr :: * -> *) backend.
                   (E.Esqueleto query expr backend, Foldable t)
         => expr (Entity Timespan)
         -> t (Key Timespan) -> [expr (E.Value Bool)]
--  | 'idFilter' makes a list of 'Esqueleto' expressions for filtering on
--  'TimespanId'
idFilter t kd = [ t ^. TimespanId E.==. val jd
                | jd <- toList kd ]

descendantLookup :: forall (m :: * -> *). (MonadIO m, Functor m)
                 => Integer -> [Key Timespan]
                 -> ReaderT E.SqlBackend m [Entity Timespan]
-- | Make a '[Entity Timespan]' with all timespans that are descended 'n'
-- links from the original 'Timespan'. 'descendantLookup' will usually
-- initially be applied to the '[Key Timespan]' the frontend asked for. It
-- then recursively calls itself n times, or until there are no more
-- descendants.
descendantLookup n ids | n <= 0    = return []
                       | null ids  = return []
                       | otherwise = do
                           jds <- join <$> (forM ids $ \ tid -> do
                                           E.select $
                                             E.from $ \t -> do
                                               E.where_ $ t ^.
                                                 TimespanParent E.==.
                                                                val (Just tid)
                                               orderBy [asc $ t ^. TimespanId]
                                               return t)
                           (jds ++) <$> descendantLookup (n - 1)
                                             (entityKey <$> jds)

timeParams :: ActionE ((ProperTime, ProperTime), (ProperTime, ProperTime))
-- | 'timeParams' gets beginMin, beginMax, endMin & endMax for a 'Timespan',
-- and deals with all the implicit stuff if something is omitted.
timeParams = do
  bMin <- paramE         "beginMin"
  bMax <- maybeParam     "beginMax"
  eMin <- maybeParam     "endMin"
  eMax <- maybeParam     "endMax"
  -- If beginMax isn't specified, set it to beginMin.
  -- If endMin isn't specified, set it to endMax.
  -- If endMax isn't specified, set it to endMin.
  -- If neither are specified, set them to beginMax.
  return ((bMin, fromMaybe bMin bMax)
         ,case (eMin, eMax) of
            (Nothing, Nothing) -> join (,) $ fromMaybe bMin bMax
            (Just a, Nothing)  -> (a, a)
            (Nothing, Just b)  -> (b, b)
            (Just a, Just b)   -> (a, b))
