{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Requests for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Spoc.Request where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types
  (
  Method,
  QueryLike,
  methodDelete,
  methodPatch,
  methodPost,
  methodPut,
  renderQuery,
  toQuery,
  )
import Network.Wai
  (
  Request,
  requestMethod,
  requestHeaders,
  )
import Network.Wai.Test
  (
  Session,
  SResponse,
  SRequest (SRequest),
  defaultRequest,
  request,
  setPath,
  srequest,
  )

import Plailude

formRequest :: Method -> Request
-- | 'formRequest' makes a blank request for use with URL-encoded form data as
-- the body.
formRequest m = defaultRequest
  { requestMethod = m
  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]}

formPostRequest :: Request
-- | 'formPostRequest' is a blank POST request for use with URL-encoded form
-- data as the body.
formPostRequest = formRequest methodPost

formPatchRequest :: Request
-- | 'formPatchRequest' is a blank PATCH request for use with URL-encoded form
-- data as the body.
formPatchRequest = formRequest methodPatch

formPutRequest :: Request
-- | 'formPutRequest' is a blank PUT request for use with URL-encoded form
-- data as the body.
formPutRequest = formRequest methodPut

bodyRequest :: Request -> B.ByteString -> L.ByteString -> Session SResponse
-- 'bodyRequest' makes a 'request' of passed in 'Request' type, with the given
-- path and URL-encoded form data as the body.
bodyRequest = srequest .: SRequest .: setPath

get :: B.ByteString -> Session SResponse
-- | 'get' makes a GET 'request' with the given path.
get = request . setPath defaultRequest

getTimespans :: (Double, Double) -> Session SResponse
-- | 'getTimespans' performs a query for timespans in the given time range.
getTimespans (a, b) =
  get $ B.append "/timespans?clock=TT&" $
    buildQuery [("begin" :: String, show a), ("end", show b)]

post :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'post' makes a POST 'request' with the given path and URL-encoded form
-- data as the body.
post = bodyRequest formPostRequest

put :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'put' makes a PUT 'request' with the given path and URL-encoded form
-- data as the body.
put = bodyRequest formPutRequest

delete :: B.ByteString -> Session SResponse
-- | 'delete' makes a DELETE 'request' with the given path.
delete = request . setPath defaultRequest { requestMethod = methodDelete }

patch :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'patch' makes a PATCH 'request' with the given path and URL-encoded form
-- data as the body.
patch = bodyRequest formPatchRequest

buildQuery :: QueryLike a => a -> B.ByteString
-- | 'buildQuery' makes a query string from a 'QueryLike', without prepending
-- a question mark.
buildQuery = renderQuery False . toQuery

buildQueryL :: QueryLike a => a -> L.ByteString
-- | 'buildQueryL' is a convenience function that makes a lazy 'L.ByteString'
-- from the result of 'buildQuery'.
buildQueryL = L.fromStrict . buildQuery
