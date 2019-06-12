{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}
{-|
Module      : Chatkit.Service.InsecureTokenProvider
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'test_token_provider' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/test-token-provider
-}
module Chatkit.Service.InsecureTokenProvider
  (
    -- * Create a new Token
    -- | See: https://pusher.com/docs/chatkit/reference/test-token-provider#create-a-token
    CreateToken(..)
  , CreateTokenResponse(..)
  )
  where

import Chatkit
import Chatkit.Model

import Pusher
import Pusher.Client
import Pusher.Client.Request

import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8
import Data.Text

-- | POST /token
data CreateToken = CreateToken
  { _userID :: UserID -- ^ The desired user id
  }
  deriving Show
-- | Response to CreateToken
data CreateTokenResponse = CreateTokenResponse
  { _accessToken :: AccessToken
  , _userID      :: UserID
  , _expiresIn   :: Int
  }
  deriving Show
instance IsRequest CreateToken CreateTokenResponse where
  toRequest (CreateToken id) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit_token_provider"
    , _version = "v1"
    , _path    = "/token"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "grant_type" .= ("client_credentials" :: Text)
       , "user_id"    .= id
       ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "Token"
    (\o -> do
      tokenType <- o .: "token_type"
      if tokenType /= ("bearer" :: Text)
        then fail "Expected token_type to be bearer"
        else CreateTokenResponse <$> o .: "access_token"
                                 <*> o .: "user_id"
                                 <*> o .: "expires_in"
    ) body

