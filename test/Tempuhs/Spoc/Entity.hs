{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

{- |
Module      :  $Header$
Description :  Entities for tests.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Spoc.Entity where

import Control.Arrow
  (
  (***),
  )
import Control.Lens
  (
  (.~),
  (^.),
  (&),
  )
import Data.Aeson
  (
  ToJSON,
  toJSON,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Maybe
  (
  isJust,
  )
import Data.Stringable
  (
  toText,
  )
import Data.Time.Clock
  (
  UTCTime,
  )
import Database.Persist
  (
  Entity (Entity),
  )
import qualified Data.Set as Z
import qualified Data.Text as T

import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  )
import Tempuhs.Spoc.Default
  (
  attributes,
  specifieds,
  )
import Tempuhs.Spoc.Type
  (
  AttributePair,
  Specified,
  )

instance HasRubbish a (Maybe UTCTime)
      => HasRubbish (Entity a) (Maybe UTCTime) where
  rubbish f (Entity a b) = Entity a <$> rubbish f b

(=^=) :: (HasRubbish a (Maybe UTCTime), ToJSON a) => [a] -> [a] -> Bool
-- | '(=^=)' takes two lists of GET results, and checks if the results in
-- the first list is the same as rubbished versions of the useful results in
-- the second result.
(a:as) =^= (b:bs) =
  let rb = b ^. rubbish
  in  isJust rb && toJSON (a & rubbish .~ rb) == toJSON b && as =^= bs
[] =^= [] = True
_  =^= _  = False

(=^^=) :: (HasRubbish a (Maybe UTCTime), HasRubbish b (Maybe UTCTime)
          ,ToJSON a, ToJSON b)
       => [(a, [b])] -> [(a, [b])] -> Bool
-- | '(=^^=)' is a special version of '(=^=)' for things with attributes.
a =^^= b = and $ (fst <$> a) =^= (fst <$> b)
               : zipWith (=^=) (snd <$> a) (snd <$> b)

clockEntity :: Integer -> T.Text -> Entity Clock
-- | 'clockEntity' is a convenience function for constructing an 'Entity'
-- containing a 'Clock'.
clockEntity k = Entity (mkKey k) . flip Clock Nothing

roleEntity :: Integer -> T.Text -> Integer -> Entity Role
-- | 'roleEntity' is a convenience function for constructing an 'Entity'
-- containing a 'Role'.
roleEntity k n ns = Entity (mkKey k) $ Role n (mkKey ns) Nothing

userEntity :: Integer -> T.Text -> Entity User
-- | 'userEntity' is a convenience function for constructing an 'Entity'
-- containing a 'User'.
userEntity k = Entity (mkKey k) . flip User Nothing

attributeEntity :: Integer -> Integer -> T.Text -> T.Text
                -> Entity TimespanAttribute
-- | 'attributeEntity' is a convenience function for constructing
-- an 'Entity' containing a 'TimespanAttribute'.
attributeEntity i f k v =
  Entity (mkKey i) $ TimespanAttribute (mkKey f) k v Nothing

timespanEntity :: Z.Set Specified -> Entity Timespan
-- | 'timespanEntity' is a convenience function for constructing
-- an 'Entity' containing a 'Timespan', based on the passed in 'Specified's.
timespanEntity ss =
  Entity (mkKey 1) $
    Timespan Nothing (mkKey 1) 10 bMax eMin eMax 1 Nothing
  where
    (bMax, eMin, eMax) =
      case ("beginMax" `Z.member` ss
           ,"endMin"   `Z.member` ss
           ,"endMax"   `Z.member` ss) of
        (False, False, False) -> (10, 10, 10)
        (True,  False, False) -> (15, 15, 15)
        (True,  True,  False) -> (15, 24, 24)
        (False, True,  False) -> (10, 24, 24)
        (True,  False, True)  -> (15, 42, 42)
        (False, True,  True)  -> (10, 24, 42)
        (False, False, True)  -> (10, 42, 42)
        (True,  True,  True)  -> (15, 24, 42)

timespansSpecsAttrs :: Z.Set Specified -> [AttributePair]
                    -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansSpecsAttrs' constructs a list of pairs. The first member is an
-- 'Entity' with a 'Timespan'. The second is a list of 'Entity's with
-- 'TimespanAttribute's. The 'Timespan' respects the passed 'Specified's. The
-- 'TimespanAttribute's respects the passed list of 'AttributePair's.
timespansSpecsAttrs ss as =
  [(timespanEntity ss
   ,mkAttributeEntities as)]

timespansSpecs :: Z.Set Specified
               -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansSpecs' does 'timespansSpecsAttrs' without 'TimespanAttribute's.
timespansSpecs = flip timespansSpecsAttrs []

timespansAttrs :: [AttributePair]
               -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansAttrs' does 'timespansSpecsAttrs' without 'Specified's
timespansAttrs = timespansSpecsAttrs Z.empty

modTimespanEntity :: (Entity Timespan, [Entity TimespanAttribute])
-- | 'modTimespanEntity' is a convenience function for constructing a pair of
-- 'Entity's, the first containing a 'Timespan' like the one in
-- 'timespanEntity' with a modified beginMin, the second containing the
-- default set of 'TimespanAttribute's in 'attributes'.
modTimespanEntity =
  (Entity (mkKey 1) $ Timespan Nothing (mkKey 1) 0 15 24 42 1 Nothing
  ,mkAttributeEntities attributes)

defaultTimespans :: [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'defaultTimespans' is a helper value for the often used
-- 'Init.initDefaultTimespan'.
defaultTimespans = timespansSpecsAttrs specifieds attributes

specialTimespan :: Integer -> Maybe Integer -> Entity Timespan
-- | 'specialTimespan' is a convenience 'Timespan' that's easy to distinguish
-- from 'defaultTimespans'. It takes the Key ID and a 'Maybe' Parent ID.
specialTimespan n p =
  Entity (mkKey n) $
         Timespan (mkKey <$> p) (mkKey 1) (-10) (-10) (-10) (-10) 1 Nothing

mkAttributeEntities :: [AttributePair] -> [Entity TimespanAttribute]
-- | 'mkAttributeEntities' takes a list of key-value pairs and makes
-- a '[Entity TimespanAttribute]'.
mkAttributeEntities as =
  [ attributeEntity i 1 k v
  | (i, (k, v)) <- [1 .. ] `zip` map (toText *** toText) as ]

defaultClock :: Entity Clock
-- | 'defaultClock' is a helper value for the often used 'Init.initClock'.
defaultClock = Entity (mkKey 1) $ Clock "TT" Nothing

defaultUser :: Entity User
-- | 'defaultUser' is a helper value for the often used
-- 'Init.initUser'.
defaultUser = Entity (mkKey 1) $ User "Luser" Nothing

defaultUserWithAttrs :: (Entity User, [Entity UserAttribute])
-- | 'defaultUserWithAttrs' is a helper value for the often used
-- 'Init.initUserAttribute'.
defaultUserWithAttrs =
  (defaultUser
  ,[Entity (mkKey 1) $ UserAttribute (mkKey 1) "name" "test" Nothing])

defaultRole :: Entity Role
-- | 'defaultRole' is a helper value for the often used
-- 'Init.initRole'.
defaultRole = Entity (mkKey 1) $ Role "Rulle" (mkKey 1) Nothing

defaultPermissionset :: Entity Permissionset
-- | 'defaultPermissionset' is a helper value for the often used
-- 'Init.initPermissionset'.
defaultPermissionset = Entity (mkKey 1) $
                       Permissionset (mkKey 1) (mkKey 1) True True True
                                     Nothing
