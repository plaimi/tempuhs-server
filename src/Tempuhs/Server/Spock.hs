{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Functions and types for tempuhs server responses and error
               handling.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Spock where

import Data.Aeson
  (
  ToJSON,
  (.=),
  object,
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Database.Persist
  (
  Key,
  )
import Network.Wai
  (
  Application,
  )
import Network.Wai.Handler.Warp
  (
  Port,
  )
import Web.Scotty.Trans
  (
  ActionT,
  ScottyError,
  ScottyT,
  json,
  scottyAppT,
  scottyT,
  showError,
  status,
  stringError,
  )

-- | An 'Error' consists of a status code, an error code and an error message
-- to be sent as a response.
data Error = MkError
  {errorStatus  :: Int    -- ^ HTTP status code.
  ,errorCode    :: T.Text -- ^ Code specifying the type of error.
  ,errorMessage :: T.Text -- ^ Human-readable error message.
  }

instance ScottyError Error where
  stringError = errInternal . T.pack
  showError (MkError s c m) = L.fromChunks [T.pack $ show s, " ", c, ": ", m]

-- | An 'ActionE' is an 'ActionT' with 'Error' as the exception type.
type ActionE = ActionT Error IO

-- | A 'ScottyE' is a 'ScottyT' with 'Error' as the exception type.
type ScottyE = ScottyT Error IO

scottyE :: Port -> ScottyE () -> IO ()
-- | 'scottyE' runs a 'ScottyE' application using the warp server.
scottyE p = scottyT p id id

scottyAppE :: ScottyE () -> IO Application
-- | 'scottyAppE' turns a 'ScottyE' application into a WAI 'Application'.
scottyAppE = scottyAppT id id

errInternal :: T.Text -> Error
-- | 'errInternal' specifies an internal server error with further details in
-- the provided message text.
errInternal = MkError 500 "INTERNAL"

errNotFound :: Error
-- | 'errNotFound' is the error given when no matching route is found.
errNotFound = MkError 404 "NOT_FOUND" "File not found"

errInvalidParam :: T.Text -> Error
-- | 'errInvalidParam' is used when the specified request parametre is
-- invalid.
errInvalidParam = MkError 400 "INVALID_PARAM" . T.append "Invalid parametre: "

errMissingParam :: T.Text -> Error
-- | 'errMissingParam' is used when the specified request parametre is
-- not found.
errMissingParam = MkError 400 "MISSING_PARAM" . T.append "Missing parametre: "

jsonPair :: ToJSON a => T.Text -> a -> ActionE ()
-- | 'jsonPair' generates a JSON object with a single attribute-value pair.
jsonPair t v = json $ object [t .= v]

jsonSuccess :: ActionE ()
-- | 'jsonSuccess' generates an empty JSON object.
jsonSuccess = json $ object []

jsonError :: Error -> ActionE ()
-- | 'jsonError' generates a JSON response from the given 'Error'.
jsonError (MkError s c m) = do
  status $ toEnum s
  jsonPair "error" $ object ["code" .= c, "message" .= m]

jsonKey :: ToJSON (Key record) => Key record -> ActionE ()
-- | 'jsonKey' generates a JSON representation of a database key.
jsonKey = jsonPair "id"
