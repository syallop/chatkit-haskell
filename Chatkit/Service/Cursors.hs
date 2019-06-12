{-# LANGUAGE EmptyDataDecls, DuplicateRecordFields, MultiParamTypeClasses, OverloadedStrings, TypeApplications, TupleSections #-}
{-|
Module      : Chatkit.Service.Cursors
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'cursors' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/cursors
-}
module Chatkit.Service.Cursors
  (
  -- * Update a Cursor
  -- | See: https://pusher.com/docs/chatkit/reference/cursors#set-a-cursor
    UpdateCursor(..)
  , UpdateCursorResponse(..)

  -- * Get a Cursor
  -- | See: https://pusher.com/docs/chatkit/reference/cursors#get-a-cursor
  , GetCursor(..)
  , GetCursorResponse(..)

  -- * Get Cursors by User
  -- | See: https://pusher.com/docs/chatkit/reference/cursors#get-cursors-by-user
  , GetUserCursors(..)
  , GetUserCursorsResponse(..)

  -- * Get Cursors by Room
  -- | See: https://pusher.com/docs/chatkit/reference/cursors#get-cursors-by-room
  , GetRoomCursors(..)
  , GetRoomCursorsResponse(..)

  -- * Subscribe to Cursors by User
  , SubscribeUserCursors(..)
  , UserCursorEvent(..)

  -- * Subscribe to Cursors by Room
  , SubscribeRoomCursors(..)
  , RoomCursorEvent(..)
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

-- | PUT /cursors/0/rooms/_roomID/users/_userID
data UpdateCursor = UpdateCursor
  { _roomID   :: RoomID
  , _userID   :: UserID
  , _position :: CursorPosition
  }
  deriving Show
-- | Response to UpdateCursor
data UpdateCursorResponse = UpdateCursorResponse
  deriving Show
instance IsRequest UpdateCursor UpdateCursorResponse where
  toRequest (UpdateCursor roomID userID position) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/rooms/" <> roomID <> "/users/" <> userID
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "position" .= position
       ]
    }
  fromResponse resp = withStatus 201 resp $ pure UpdateCursorResponse

-- | GET /cursors/0/rooms/_roomID/users/_userID
data GetCursor = GetCursor
  { _roomID :: RoomID
  , _userID :: UserID
  }
  deriving Show
-- | Response to GetCursor
data GetCursorResponse = GetCursorResponse
  { _cursor :: Maybe Cursor
  }
  deriving Show
-- If no cursor was set, null is returned. Is this handled by the default Maybe
-- parsing?
instance IsRequest GetCursor GetCursorResponse where
  toRequest (GetCursor roomID userID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/rooms/" <> roomID <> "/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetCursorResponse <$> parseJSON body

-- | GET /cursors/0/users/_userID
data GetUserCursors = GetUserCursors
  { _userID :: UserID
  }
  deriving Show
-- | Response to GetUserCursors
data GetUserCursorsResponse = GetUserCursorsResponse
  { _cursors :: [Cursor]
  }
  deriving Show
instance IsRequest GetUserCursors GetUserCursorsResponse where
  toRequest (GetUserCursors userID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUserCursorsResponse <$> parseJSONList body

-- | GET /cursors/0/rooms/_roomID
data GetRoomCursors = GetRoomCursors
  { _roomID :: RoomID
  }
  deriving Show
-- | Response to GetRoomCursors
data GetRoomCursorsResponse = GetRoomCursorsResponse
  { _cursors :: [Cursor]
  }
  deriving Show
instance IsRequest GetRoomCursors GetRoomCursorsResponse where
  toRequest (GetRoomCursors roomID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/rooms/" <> roomID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRoomCursorsResponse <$> parseJSONList body

-- | SUBSCRIBE /cursors/0/users/_userID
data SubscribeUserCursors = SubscribeUserCursors
  { _userID :: UserID
  }
  deriving Show
instance IsSubscribe SubscribeUserCursors UserCursorEvent where
  toSubscribeRequest (SubscribeUserCursors userID) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/users/" <> userID
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

-- | SUBSCRIBE /cursors/0/rooms/_roomID
data SubscribeRoomCursors = SubscribeRoomCursors
  { _roomID :: RoomID
  }
  deriving Show
instance IsSubscribe SubscribeRoomCursors RoomCursorEvent where
  toSubscribeRequest (SubscribeRoomCursors roomID) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit_cursors"
    , _version = "v2"
    , _path    = "/cursors/0/rooms/" <> roomID
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

