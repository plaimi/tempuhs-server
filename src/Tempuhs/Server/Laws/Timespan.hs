{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- |
Module      :  $Header$
Description :  Timespan law enforcers. See /docs/LAWS.txt.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Laws.Timespan where

import Control.Monad
  (
  void,
  when,
  )
import Control.Monad.IO.Class
  (
  MonadIO,
  )
import Control.Monad.Trans.Class
  (
  lift,
  )
import Control.Monad.Trans.Maybe
  (
  MaybeT (MaybeT),
  runMaybeT,
  )
import Control.Monad.Trans.Reader
  (
  ReaderT,
  )
import Database.Esqueleto
  (
  SqlBackend,
  )
import Database.Persist
  (
  Key (Key),
  (=.),
  entityVal,
  get,
  update,
  )

import Tempuhs.Chronology
import Tempuhs.Server.Laws.Props
  (
  beginMinProp,
  endMaxProp,
  isFlexProp,
  isFlexibleProp,
  parentCycleProp,
  )
import Tempuhs.Server.Requests.Timespan.Util
  (
  descendantLookup,
  )

flexTimespan :: forall (m :: * -> *). (MonadIO m, Functor m)
             => Key Timespan -> ReaderT SqlBackend m ()
-- | 'flexTimespan' takes a 'Key Timespan' and makes sure that it, and its
-- parent, is respecting the FlexLaws.
flexTimespan t = void . runMaybeT $ do
  u <- MaybeT $ get t
  c <- MaybeT $ get (timespanClock u)
  when (isFlexibleProp u && isFlexProp c) $ lift $ fixFlex t

fixFlex :: forall (m :: * -> *). (MonadIO m, Functor m)
        => Key Timespan -> ReaderT SqlBackend m ()
-- | 'fixFlex' makes sure the 'Timespan' with the passed in 'Key Timespan'
-- follows the FlexLaws. It then checks the parent of the 'Timespan' as well.
fixFlex t = do
  ds <- descendantLookup 1 [t]
  flexPerChildren t (map entityVal ds)
  flexParent t

flexPerChildren :: forall (m :: * -> *). MonadIO m
                => Key Timespan -> [Timespan] -> ReaderT SqlBackend m ()
-- | 'flexPerChildren' looks up the children of a 'Timespan' using its 'Key
-- Timespan', and makes sure it follows the FlexLaws.
flexPerChildren _ []  = return ()
flexPerChildren t ds  = update t [TimespanBeginMin =. b, TimespanEndMax =. e]
  where (b, e) = (beginMinProp ds
                 ,endMaxProp ds)

flexParent :: forall (m :: * -> *). (MonadIO m, Functor m)
           => Key Timespan -> ReaderT SqlBackend m ()
-- | 'flexParent' takes a 'Key Timespan' and makes sure that its parent, is
-- respecting the FlexLaws.
flexParent t = void . runMaybeT $ do
  u <- MaybeT $ get t
  p <- MaybeT . return $ timespanParent u
  r <- MaybeT $ get p
  c <- MaybeT $ get (timespanClock r)
  when (isFlexibleProp u && isFlexProp c) $ lift $ fixFlex p

parentCycle :: forall (m :: * -> *). MonadIO m
            => [Key Timespan]
            -> Key Timespan -> ReaderT SqlBackend m (Maybe Bool)
-- | 'parentCycle' checks if there is an illegal cycle of 'TimespanParent's
-- per the ParentLaws.
parentCycle ts p = runMaybeT $
  if not $ parentCycleProp p ts
    then return True
    else do
      q <- MaybeT $ get p
      let r = timespanParent q
      case r of
        Nothing -> return False
        Just s  -> MaybeT $ parentCycle (p : ts) s
