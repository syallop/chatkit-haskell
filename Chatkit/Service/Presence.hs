{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit.Service.Presence
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'presence' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/presence
|-}
module Chatkit.Service.Presence
  (
   -- * Register as online
   -- | See: https://pusher.com/docs/chatkit/reference/presence#register-as-online
    SubscribeRegisterOnline(..)
  , RegisterOnlineEvent(..)

  -- * Listen to a Users state
  , SubscribeUserPresence(..)
  , UserPresenceEvent(..)
  )
  where

import Chatkit
import Chatkit.Model

import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 hiding (foldr)
import Data.Maybe
import qualified Data.Text as Text

-- | SUBSCRIBE /users/_userID/register
data SubscribeRegisterOnline = SubscribeRegisterOnline
  { _userID :: UserID
  }
  deriving Show
instance IsSubscribe SubscribeRegisterOnline RegisterOnlineEvent where
  toSubscribeRequest (SubscribeRegisterOnline userID) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit_presence"
    , _version = "v2"
    , _path    = "/users/" <> userID <> "/register"
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

-- | SUBSCRIBE /users/_userID
data SubscribeUserPresence = SubscribeUserPresence
  { _userID :: UserID
  }
  deriving Show
instance IsSubscribe SubscribeUserPresence UserPresenceEvent where
  toSubscribeRequest (SubscribeUserPresence userID) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit_presence"
    , _version = "v2"
    , _path    = "/users/" <> userID
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

