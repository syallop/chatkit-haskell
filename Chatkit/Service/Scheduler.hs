{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit.Service.Scheduler
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses to the 'Scheduler' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/scheduler
-}
module Chatkit.Service.Scheduler
  (
  -- * Create a job to asynchronously delete a single User by their UserID
  -- | See: https://pusher.com/docs/chatkit/reference/scheduler#delete-a-user
    CreateDeleteUserJob(..)
  , CreateDeleteUserJobResponse(..)

  -- * Create a job to asynchronously delete a single Room by their RoomID
  -- | See: https://pusher.com/docs/chatkit/reference/scheduler#delete-a-room
  , CreateDeleteRoomJob(..)
  , CreateDeleteRoomJobResponse(..)

  -- * Get the status of a job by it's JobID
  -- | See: https://pusher.com/docs/chatkit/reference/scheduler#job-status
  , GetJobStatus(..)
  , GetJobStatusResponse(..)
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

-- Users

-- | PUT /users/_userID
data CreateDeleteUserJob = CreateDeleteUserJob
  { _userID :: UserID
  }
  deriving Show
-- | Response to CreateDeleteUserJob
data CreateDeleteUserJobResponse = CreateDeleteUserJobResponse
  { _jobID     :: JobID
  }
  deriving Show
instance IsRequest CreateDeleteUserJob CreateDeleteUserJobResponse where
  toRequest (CreateDeleteUserJob userID) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit_scheduler"
    , _version = "v1"
    , _path    = "/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "Job" (\o -> CreateDeleteUserJobResponse <$> o .: "id") body

-- | PUT /rooms/_roomID
data CreateDeleteRoomJob = CreateDeleteRoomJob
  { _roomID :: RoomID
  }
  deriving Show
-- | Response to CreateDeleteRoomJob
data CreateDeleteRoomJobResponse = CreateDeleteRoomJobResponse
  { _jobID     :: JobID
  }
  deriving Show
instance IsRequest CreateDeleteRoomJob CreateDeleteRoomJobResponse where
  toRequest (CreateDeleteRoomJob roomID) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit_scheduler"
    , _version = "v1"
    , _path    = "/rooms/" <> roomID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "Job" (\o -> CreateDeleteRoomJobResponse <$> o .: "id") body

data GetJobStatus = GetJobStatus
  { _jobID     :: JobID
  }
  deriving Show
data GetJobStatusResponse = GetJobStatusResponse
  { _jobID     :: JobID
  , _jobStatus :: JobStatus
  }
  deriving Show
instance IsRequest GetJobStatus GetJobStatusResponse where
  toRequest (GetJobStatus jobID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_scheduler"
    , _version = "v1"
    , _path    = "/status/" <> jobID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "Job" (\o -> GetJobStatusResponse <$> o .: "id" <*> o .: "status") body

