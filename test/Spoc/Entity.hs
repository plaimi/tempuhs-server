{- |
Module      :  $Header$
Description :  Entities for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Entity where

import Data.Aeson
  (
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
import Database.Persist
  (
  Entity (Entity),
  )
import qualified Data.Set as Z
import qualified Data.Text as T

import Plailude
import Spoc.Default
  (
  attributes,
  specifieds,
  )
import Spoc.Type
  (
  AttributeKey,
  AttributeValue,
  Specified,
  )
import Tempuhs.Chronology
import Tempuhs.Server.Database
  (
  mkKey,
  )

clockEntity :: Integer -> T.Text -> Entity Clock
-- | 'clockEntity' is a convenience function for constructing an 'Entity'
-- containing a 'Clock'.
clockEntity k = Entity (mkKey k) . Clock

attributeEntity :: Integer -> Integer -> T.Text -> T.Text ->
                   Entity TimespanAttribute
-- | 'attributeEntity' is a convenience function for constructing
-- an 'Entity' containing a 'TimespanAttribute'.
attributeEntity k = Entity (mkKey k) .:. (TimespanAttribute . mkKey)

timespanEntity :: Z.Set Specified -> Entity Timespan
-- | 'timespanEntity' is a convenience function for constructing
-- an 'Entity' containing a 'Timespan', based on the passed in 'Specified's.
timespanEntity ss =
  Entity (mkKey 1) $
    Timespan Nothing (mkKey 1) 10 beginMax endMin endMax 1 Nothing
  where
    (beginMax, endMin, endMax) =
      case ("beginMax" `Z.member` ss
           ,"endMin"   `Z.member` ss
           ,"endMax"   `Z.member` ss) of
        (False, False, False) -> (11, 10, 11)
        (True,  False, False) -> (15, 10, 15)
        (True,  True,  False) -> (15, 24, 25)
        (False, True,  False) -> (11, 24, 25)
        (True,  False, True)  -> (15, 41, 42)
        (False, True,  True)  -> (11, 24, 42)
        (False, False, True)  -> (11, 41, 42)
        (True,  True,  True)  -> (15, 24, 42)

timespansSpecsAttrs :: Z.Set Specified
                   -> [(AttributeKey, AttributeValue)]
                   -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansSpecsAttrs' constructs a list of pairs. The first member is an
-- 'Entity' with a 'Timespan'. The second is a list of 'Entity's with
-- 'TimespanAttribute's. The 'Timespan' respects the passed 'Specified's. The
-- 'TimespanAttribute's respects the passed list of pairs of 'AttributeKey's
-- and 'AttributeValue's.
timespansSpecsAttrs ss as =
  [(timespanEntity ss
   ,[ attributeEntity i 1 k v
    | (i, (k, v)) <- [1 .. ] `zip` map (both T.pack) as ])]

timespansSpecs :: Z.Set Specified
                   -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansSpecs' does 'timespansSpecsAttrs' without 'TimespanAttribute's.
timespansSpecs = flip timespansSpecsAttrs []

timespansAttrs :: [(AttributeKey, AttributeValue)]
                   -> [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'timespansAttrs' does 'timespansSpecsAttrs' without 'Specified's
timespansAttrs = timespansSpecsAttrs Z.empty

defaultTimespans:: [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'defaultTimespans' is a helper value for the often used
-- 'Init.initDefaultTimespan'.
defaultTimespans = timespansSpecsAttrs specifieds attributes

specialTimespan :: Maybe Integer -> Entity Timespan
-- | 'specialTimespan' is a convenience 'Timespan' that's easy to distinguish
-- from 'defaultTimespans'.
specialTimespan p =
  Entity (mkKey 2) $
         Timespan (mkKey <$> p) (mkKey 1) (-10) (-9) (-10) (-9) 1 Nothing

rubbishP :: [(Entity Timespan, [Entity TimespanAttribute])] ->
            [(Entity Timespan, [Entity TimespanAttribute])] ->
            Bool
-- | 'rubbishP' takes two GET /timespans results, and checks if the 'Timespan'
-- within the second result is the same as a rubbished version of the useful
-- timespan in the first result.
rubbishP ((Entity ek ev,_):es) ((f@(Entity _ fv),_):fs) =
  let fr = timespanRubbish fv
  in  isJust fr                                                &&
      toJSON (Entity ek ev {timespanRubbish = fr}) == toJSON f &&
      rubbishP es fs
rubbishP [] []                                          = True
rubbishP _  []                                          = False
rubbishP []  _                                          = False
