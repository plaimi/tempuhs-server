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
  specifieds,
  )
import Spoc.Type
  (
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

firstTimespanEntity :: Z.Set Specified -> Entity Timespan
-- | 'firstTimespanEntity' is equal to the data inserted by
-- 'Init.initTimespan' with the appropriate behaviour depending on which
-- optionals are specified in the 'Z.Set' of 'Specified's.
firstTimespanEntity ss =
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

modTimespanEntity :: Entity Timespan
-- | 'modTimespanEntity' is equal to the data inserted by
-- 'Init.initModTimespan'.
modTimespanEntity =
  Entity (mkKey 1) $ Timespan Nothing (mkKey 1) 0 1 9 10 1 Nothing

subTimespanEntity :: Entity Timespan
-- | 'subTimespanEntity' is equal to the data inserted by
-- 'Init.initSubTimespan'.
subTimespanEntity =
  Entity (mkKey 2) $
    Timespan (Just $ mkKey 1) (mkKey 1) (-9) (-8) 8 9 1 Nothing

firstTimespans :: Z.Set Specified -> [Entity TimespanAttribute] ->
                  [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'firstTimespans' is the expected response for a timespan query that
-- matches what 'Init.initTimespan' inserted, depending on which fields are to
-- be left unspecified per the 'Z.Set' of 'Specified's, and the given
-- '[Entity TimespanAttribute]'.
firstTimespans ss attrs = [(firstTimespanEntity ss, attrs)]

defaultTimespans :: [(Entity Timespan, [Entity TimespanAttribute])]
-- | 'defaultTimespans' is a helper value for the often used 'firstTimespans'
-- with 'specifedsSet' and '[]'.
defaultTimespans = firstTimespans specifieds []

rubbishP :: [(Entity Timespan, [Entity TimespanAttribute])] ->
            [(Entity Timespan, [Entity TimespanAttribute])] ->
            Bool
-- | 'rubbishP' takes two get /timespan-results, and checks if the 'Timespan'
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
