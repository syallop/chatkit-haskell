{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit.Service.Undocumented
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

Implements undocumented endpoints in the Chatkit API.
|-}

module Chatkit.Service.Undocumented
  ( -- * Delete all instance resources
    DeleteResources(..)
  , DeleteResourcesResponse(..)
  )
  where

import Chatkit
import Chatkit.Model

import Pusher
import Pusher.Client
import Pusher.Client.Request

import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 hiding (foldr)
import Data.Maybe
import qualified Data.Text as Text

-- | DELETE /resources
data DeleteResources = DeleteResources
  deriving Show
-- | Response to DeleteResources
data DeleteResourcesResponse = DeleteResourcesResponse
  deriving Show
instance IsRequest DeleteResources DeleteResourcesResponse where
  toRequest DeleteResources = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit"
    , _version = "v4"
    , _path    = "/resources"
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteResourcesResponse

