{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Requests for tests.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Spoc.Request where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types
  (
  QueryLike,
  methodDelete,
  methodPost,
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

formPostRequest :: Request
-- | 'formPostRequest' is a blank POST request for use with URL-encoded form
-- data as the body.
formPostRequest = defaultRequest
  { requestMethod = methodPost
  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]}

get :: B.ByteString -> Session SResponse
-- | 'get' makes a GET 'request' with the given path.
get = request . setPath defaultRequest

getTimespans :: (Double, Double) -> Session SResponse
-- | 'getTimespans' performs a query for timespans in the given time range.
getTimespans (a, b) =
  get $ B.append "/timespans?clock=TT&" $
    buildQuery [("begin" :: String, show a), ("end", show b)]

delete :: B.ByteString -> Session SResponse
-- | 'delete' makes a DELETE 'request' with the given path.
delete = request . setPath defaultRequest { requestMethod = methodDelete }

post :: B.ByteString -> L.ByteString -> Session SResponse
-- | 'post' makes a POST 'request' with the given path and URL-encoded form
-- data as the body.
post p b = srequest $ SRequest (setPath formPostRequest p) b

buildQuery :: QueryLike a => a -> B.ByteString
-- | 'buildQuery' makes a query string from a 'QueryLike', without prepending
-- a question mark.
buildQuery = renderQuery False . toQuery

buildQueryL :: QueryLike a => a -> L.ByteString
-- | 'buildQueryL' is a convenience function that makes a lazy 'L.ByteString'
-- from the result of 'buildQuery'.
buildQueryL = L.fromStrict . buildQuery
