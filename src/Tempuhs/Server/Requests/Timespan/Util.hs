{-# LANGUAGE KindSignatures    #-}
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
import qualified Database.Esqueleto as E
import Database.Esqueleto
  (
  (^.),
  (<=.),
  (>=.),
  asc,
  from,
  isNothing,
  orderBy,
  val,
  where_,
  )
import qualified Database.Esqueleto.Internal.Language
  (
  From,
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
  )
import Tempuhs.Server.Database
  (
  mkKey,
  )

joinList :: forall (m :: * -> *) a b (expr :: * -> *) backend.
                   Database.Esqueleto.Internal.Language.From m expr backend a
         => [b -> a -> expr (E.Value Bool)] -> m b -> m b
-- | 'joinList' takes two lists of Esqueleto expressions. The first is a list
-- of expressions to SQL JOIN to every member of the second list, typically
-- a list of SQL SELECTs.
joinList (e:es) t = joinList es t >>= \t' -> from $ \b -> where_ (e t' b)
                                                       >> return t'
joinList []     t = t

cmpMaybe :: forall t (query :: * -> *) (expr :: * -> *)
                     backend (query1 :: * -> *) (expr1 :: * -> *)
                     backend1 typ.
                     (E.Esqueleto query1 expr1 backend1
                     ,E.Esqueleto query expr backend
                     ,E.PersistField typ
                     ,E.PersistField t)
          => (expr1 (E.Value (Maybe typ))
          -> expr (E.Value (Maybe t))
          -> expr1 (E.Value Bool))
          -> expr1 (E.Value (Maybe typ))
          -> Maybe t
          -> expr1 (E.Value Bool)
-- | If b is 'Just', 'cmpMaybe' applies the passed in function to a, and gets
-- the value of b. If b is 'Nothing', 'cmpMaybe' checks if a is 'Nothing'.
cmpMaybe f a b@(Just _) = f a $ val b
cmpMaybe _ a _          = isNothing a

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
