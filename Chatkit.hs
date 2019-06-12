{-# LANGUAGE
    DeriveFunctor
  , DeriveAnyClass
  , DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

Defines a 'Chatkit' type for chaining computations that make requests/subscriptions to Chatkit services.
The underlying HTTP2 Client(s) are shared across requests and so requests are made as streams over the same
connection.

Re-exports:

- A Chatkit data model from Chatkit.Service.*
- A minimal HTTP2 Client from Chatkit.Client

See the README/ package description/ https://pusher.com/docs/chatkit/reference/api
for more information.
-}

module Chatkit
  (
  -- * Create an Environment for making Chatkit requests
    PusherEnv(..)
  , mkPusherEnv

  -- * Requests and Subscriptions to Chatkit may be made within this type
  , Pusher()
  , PusherResult(..)
  , runPusher
  , pusherIO
  , pusherFail
  , pusherErrorResponse
  , pusherTry
  , getEnv
  , pusherRequest
  , pusherSubscribe
  , pusherReadRawEvent
  , pusherReadEvent

  -- * Re-export
  , module Pusher
  , module Pusher.Client
  , module Pusher.Client.Request
  , module Pusher.Client.Subscribe
  , module Pusher.Model
  , module Chatkit.Model
  )
  where

import Pusher
import Pusher.Client
import Pusher.Client.Request
import Pusher.Client.Subscribe
import Pusher.Model
import Chatkit.Model

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString
import Data.Default.Class
import Data.IORef
import Data.Map (Map)
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.Time.Clock
import Data.Vector
import Network.HPACK
import Network.HTTP2
import Network.HTTP2.Client
import Network.HTTP2.Client.Helpers
import Network.TLS as TLS hiding (sendData, Header)
import Network.TLS.Extra.Cipher as TLS
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy  as L
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Web.JWT as JWT

import System.Timeout

