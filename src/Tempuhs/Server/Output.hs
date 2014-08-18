{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Functions for generating tempuhs server responses.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Server.Output where

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
  KeyBackend
  )
import Web.Scotty
  (
  ActionM,
  json,
  status
  )

-- | An 'Error' consists of a status code, an error code and an error message
-- to be sent as a response.
data Error = Error
  {errorStatus  :: Int    -- ^ HTTP status code.
  ,errorCode    :: T.Text -- ^ Code specifying the type of error.
  ,errorMessage :: T.Text -- ^ Human-readable error message.
  }

errInternal :: T.Text -> Error
-- | 'errInternal' specifies an internal server error with further details in
-- the provided message text.
errInternal = Error 500 "INTERNAL"

errNotFound :: Error
-- | 'errNotFound' is the error given when no matching route is found.
errNotFound = Error 404 "NOT_FOUND" "File not found"

errInvalidParam :: T.Text -> Error
-- | 'errInvalidParam' is used when the specified request parameter is
-- invalid.
errInvalidParam = Error 400 "INVALID_PARAM" . T.append "Invalid parameter: "

jsonPair :: ToJSON a => T.Text -> a -> ActionM ()
-- | 'jsonPair' generates a JSON object with a single attribute-value pair.
jsonPair t v = json $ object [t .= v]

jsonSuccess :: ActionM ()
-- | 'jsonSuccess' generates an empty JSON object.
jsonSuccess = json $ object []

jsonError :: Error -> ActionM ()
-- | 'jsonError' generates a JSON response from the given 'Error'.
jsonError (Error s c m) = do
  status $ toEnum s
  jsonPair "error" $ object ["code" .= c, "message" .= m]

jsonKey :: KeyBackend backend entity -> ActionM ()
-- | 'jsonKey' generates a JSON representation of a database key.
jsonKey = jsonPair "id"

jsonHandler :: L.Text -> ActionM ()
-- | 'jsonHandler' is used to turn any unhandled errors into JSON-encoded
-- error responses.
jsonHandler = jsonError . errInternal . L.toStrict
