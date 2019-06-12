{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit.Service.Core
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'Core' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/api-v6
-}
module Chatkit.Service.Core
  (
  -- * Creation of single Users
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#create-a-user
    CreateUser(..)
  , CreateUserResponse(..)

  -- * Creation of multiple Users
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#batch-create-users
  , CreateUsers(..)
  , CreateUsersResponse(..)

  -- * Retrieve a single User by UserID
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#get-user
  , GetUser(..)
  , GetUserResponse(..)

  -- * Retrieve multiple Users, optionally by age.
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#get-users
  , GetUsers(..)
  , GetUsersResponse(..)

  -- * Retrieve multiple Users by their UserIDs
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#get-users-by-ids
  , GetUsersByIDs(..)
  , GetUsersByIDsResponse(..)

  -- * Update a single User by their UserIDs
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#update-a-user
  , UpdateUser(..)
  , UpdateUserResponse(..)

  -- * Delete a single User by their UserID
  -- | See: https://pusher.com/docs/chatkit/reference/api-v4#delete-a-user
  , DeleteUser(..)
  , DeleteUserResponse(..)

  -- * Get the Rooms a User is a member of
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#get-user-rooms
  , GetUserRooms(..)
  , GetUserRoomsResponse(..)

  -- * Join a User to a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#join-a-room
  , CreateJoinRoom(..)
  , CreateJoinRoomResponse(..)

  -- * Leave a User from a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#leave-a-room
  , CreateLeaveRoom(..)
  , CreateLeaveRoomResponse(..)

  -- * Subscribe to User events
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#subscribe-to-user-events
  , SubscribeUsers(..)
  , UserEvent(..)

  -- * Create a new Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#create-a-room
  , CreateRoom(..)
  , CreateRoomResponse(..)

  -- * Retrieve a Room by RoomID
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#fetch-a-room
  , GetRoom(..)
  , GetRoomResponse(..)

  -- * Retrieve multiple Rooms
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#fetch-rooms
  , GetRooms(..)
  , GetRoomsResponse(..)

  -- * Delete a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v4#delete-a-room
  , DeleteRoom(..)
  , DeleteRoomResponse(..)

  -- * Update a Room by its RoomID
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#update-a-room
  , UpdateRoom(..)
  , UpdateRoomResponse(..)

  -- * Add multiple Users to a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#add-users
  , UpdateAddUsers(..)
  , UpdateAddUsersResponse(..)

  -- * Remove multiple Users from a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#remove-users
  , UpdateRemoveUsers(..)
  , UpdateRemoveUsersResponse(..)

  -- * Subscribe to events on a single Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#subscribe-to-a-room
  , SubscribeRoom(..)
  , RoomEvent(..)

  -- * Subscribe to Room membership events on a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#subscribe-to-room-memberships
  , SubscribeRoomMemberships(..)
  , RoomMembershipEvent(..)

  -- * Create a typing indicator on a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#typing-indicators
  , CreateTypingIndicators(..)
  , CreateTypingIndicatorsResponse(..)

  -- * Create a new message on a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#send-a-message
  , CreateMessage(..)
  , CreateMessageResponse(..)

  -- * Create an attachment for a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#upload-an-attachment
  , CreateAttachment(..)
  , CreateAttachmentResponse(..)

  -- * Get Messages on a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#fetch-messages-from-a-room
  , GetRoomMessages(..)
  , GetRoomMessagesResponse(..)

  -- * Get a Message from a Room
  -- | See: https://pusher.com/docs/chatkit/reference/api-v6#fetch-a-single-message-from-a-room
  , GetRoomMessage(..)
  , GetRoomMessageResponse(..)

  -- * Update a Message in a Room
  -- | See: https://pusher.com/docs/chatkit/reference/latest#edit-a-message
  , EditRoomMessage(..)
  , EditRoomMessageResponse(..)

  -- * Delete a single Message
  -- | See: https://pusher.com/docs/chatkit/reference/api-v4#delete-a-message
  , DeleteMessage(..)
  , DeleteMessageResponse(..)
  )
  where

import Chatkit
import Chatkit.Model

import Pusher
import Pusher.Client
import Pusher.Client.Request

import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 hiding (foldr)
import Data.Maybe
import qualified Data.Text as Text

-- Users

-- | POST /users
data CreateUser = CreateUser
  { _userID     :: UserID
  , _userName   :: UserName
  , _avatarURL  :: Maybe URL
  , _customData :: Maybe CustomData
  }
  deriving Show
-- | Response to CreateUser
data CreateUserResponse = CreateUserResponse
  { _user :: User
  }
  deriving Show
instance IsRequest CreateUser CreateUserResponse where
  toRequest (CreateUser id name mAvatarURL mCustomData) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "id"          .= id
       , "name"        .= name
       , "avatar_url"  .= mAvatarURL
       , "custom_data" .= mCustomData
       ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ CreateUserResponse <$> parseJSON body

-- | POST /batch_users
data CreateUsers = CreateUsers
  { _users :: [CreateUser]
  }
  deriving Show
-- | Response to CreateUsers
data CreateUsersResponse = CreateUsersResponse
  { _users :: [User]
  }
  deriving Show
instance IsRequest CreateUsers CreateUsersResponse where
  toRequest (CreateUsers createUsers) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/batch_users"
    , _body    = Just . toStrict
                      . encodingToLazyByteString
                      . pairs
                      . pair "users"
                      . list (\(CreateUser id name mAvatarURL mCustomData) -> pairs . mconcat $
                                [ "id"          .= id
                                , "name"        .= name
                                , "avatar_url"  .= mAvatarURL
                                , "custom_data" .= mCustomData
                                ]
                             )
                      $ createUsers
    }

  fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ CreateUsersResponse <$> parseJSONList body

-- | GET /users/_userID
data GetUser = GetUser
  { _userID :: UserID
  }
  deriving Show
-- | Response to GetUser
data GetUserResponse = GetUserResponse
  { _user :: User
  }
  deriving Show
instance IsRequest GetUser GetUserResponse where
  toRequest (GetUser userID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUserResponse <$> parseJSON body

-- | GET /users
data GetUsers = GetUsers
  { _fromTimestamp :: Maybe Timestamp
  , _limit         :: Maybe Limit
  }
  deriving Show
-- | Response to GetUsers
data GetUsersResponse = GetUsersResponse
  { _users :: [User]
  }
  deriving Show
instance IsRequest GetUsers GetUsersResponse where
  toRequest req = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users"
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUsersResponse <$> parseJSONList body

-- | GET /users_by_ids
data GetUsersByIDs = GetUsersByIDs
  { _userIDs :: [UserID]
  }
  deriving Show
-- | Response to GetUserByIds
data GetUsersByIDsResponse = GetUsersByIDsResponse
  { _users :: [User]
  }
  deriving Show
instance IsRequest GetUsersByIDs GetUsersByIDsResponse where
  toRequest (GetUsersByIDs userIDs) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users_by_ids?id=" <> Text.intercalate "&id=" userIDs
    , _body    = Nothing
    }
    where
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUsersByIDsResponse <$> parseJSONList body

-- | PUT /users/_userID
data UpdateUser = UpdateUser
  { _userID     :: UserID
  , _userName   :: Maybe UserName
  , _avatarURL  :: Maybe URL
  , _customData :: Maybe CustomData
  }
  deriving Show
-- | Response to UpdateUser
data UpdateUserResponse = UpdateUserResponse
  deriving Show
instance IsRequest UpdateUser UpdateUserResponse where
  toRequest (UpdateUser userID mUserName mAvatarURL mCustomData) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users/" <> userID
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "name"        .= mUserName
       , "avatar_url"  .= mAvatarURL
       , "custom_data" .= mCustomData
       ]
    }
  fromResponse resp = withStatus 204 resp $ pure UpdateUserResponse

-- | DELETE /users/_userID
data DeleteUser = DeleteUser
  { _userID :: UserID
  }
  deriving Show
-- | Response to DeleteUser
data DeleteUserResponse = DeleteUserResponse
  deriving Show
instance IsRequest DeleteUser DeleteUserResponse where
  toRequest (DeleteUser userID)= Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit"
    , _version = "v4"
    , _path    = "/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteUserResponse

-- | GET /users/_userID/rooms
data GetUserRooms = GetUserRooms
  { _userID   :: UserID
  , _joinable :: Maybe Bool
  }
  deriving Show
-- | Response to GetUserRooms
data GetUserRoomsResponse = GetUserRoomsResponse
  { _rooms :: [Room]
  }
  deriving Show
instance IsRequest GetUserRooms GetUserRoomsResponse where
  toRequest (GetUserRooms userID joinable) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users/" <> userID <> "/rooms" <> maybe "" (\j -> "?joinable=" <> if j then "true" else "false") joinable
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUserRoomsResponse <$> parseJSONList body

-- | POST /users/_userID/rooms/_roomID/join
data CreateJoinRoom = CreateJoinRoom
  { _userID :: UserID
  , _roomID :: RoomID
  }
  deriving Show
-- | Response to CreateJoinRoom
data CreateJoinRoomResponse = CreateJoinRoomResponse
  { _joinedRoom :: JoinedRoom
  }
  deriving Show
-- | Issue a CreateJoinRoom request.
instance IsRequest CreateJoinRoom CreateJoinRoomResponse where
  toRequest (CreateJoinRoom userID roomID) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users/" <> userID <> "/rooms/" <> roomID <> "/join"
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ CreateJoinRoomResponse <$> parseJSON body

-- | POST /users/_userID/rooms/_roomID/leave
data CreateLeaveRoom = CreateLeaveRoom
  { _userID :: UserID
  , _roomID :: UserID
  }
  deriving Show
-- | Response to CreateLeaveRoom
data CreateLeaveRoomResponse = CreateLeaveRoomResponse
  deriving Show
instance IsRequest CreateLeaveRoom CreateLeaveRoomResponse where
  toRequest (CreateLeaveRoom userID roomID) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users/" <> userID <> "/rooms/" <> roomID <> "/leave"
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure CreateLeaveRoomResponse

-- | SUBSCRIBE /users
data SubscribeUsers = SubscribeUsers
  deriving Show
instance IsSubscribe SubscribeUsers UserEvent where
  toSubscribeRequest SubscribeUsers = SubscriptionRequest
    { _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users"
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

-- Rooms

-- | POST /rooms
data CreateRoom = CreateRoom
  { _roomID     :: Maybe RoomID
  , _roomName   :: Maybe RoomName
  , _private    :: Maybe Bool
  , _customData :: Maybe CustomData
  , _userIDs    :: Maybe [UserID]
  }
  deriving Show
-- | Response to CreateRoom
data CreateRoomResponse = CreateRoomResponse
  { _room :: Room
  }
  deriving Show
instance IsRequest CreateRoom CreateRoomResponse where
  toRequest (CreateRoom mRoomID mRoomName mPrivate mCustomData mUserIDs) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "id"          .= mRoomID
       , "name"        .= mRoomName
       , "private"     .= mPrivate
       , "custom_data" .= mCustomData
       , "user_ids"    .= mUserIDs
       ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ CreateRoomResponse <$> parseJSON body

-- | GET /rooms/_roomID
data GetRoom = GetRoom
  { _roomID :: RoomID
  }
  deriving Show
-- | Response to GetRoom
data GetRoomResponse = GetRoomResponse
  { _room :: Room
  }
  deriving Show
instance IsRequest GetRoom GetRoomResponse where
  toRequest (GetRoom roomID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRoomResponse <$> parseJSON body

-- | GET /rooms/
data GetRooms = GetRooms
  { _fromID         :: Maybe RoomID
  , _includePrivate :: Maybe Bool
  }
  deriving Show
-- | Response to GetRooms
data GetRoomsResponse = GetRoomsResponse
  { _rooms :: [Room]
  }
  deriving Show
instance IsRequest GetRooms GetRoomsResponse where
  toRequest (GetRooms mFromID mIncludePrivate) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms" <> queryString
    , _body    = Nothing
    }
    where
      queryString = foldr (\(k,v) acc -> acc <> "&" <> k <> "=" <> v) "?" . catMaybes $
                      maybe Nothing (Just . ("from_id",))         mFromID
                    : maybe Nothing (Just . ("include_private",) . Text.pack . show) mIncludePrivate
                    : []
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRoomsResponse <$> parseJSONList body

-- | DELETE /rooms/_roomID
data DeleteRoom = DeleteRoom
  { _roomID :: RoomID
  }
  deriving Show
-- | Response to DeleteRoom
data DeleteRoomResponse = DeleteRoomResponse
  deriving Show
instance IsRequest DeleteRoom DeleteRoomResponse where
  toRequest (DeleteRoom roomID) = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit"
    , _version = "v4"
    , _path    = "/rooms/" <> roomID
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteRoomResponse

-- | PUT /rooms/_roomID
data UpdateRoom = UpdateRoom
  { _roomID     :: RoomID
  , _roomName   :: Maybe RoomName
  , _private    :: Maybe Bool
  , _customData :: Maybe CustomData
  }
  deriving Show
-- | Response to UpdateRoom
data UpdateRoomResponse = UpdateRoomResponse
  deriving Show
instance IsRequest UpdateRoom UpdateRoomResponse where
  toRequest (UpdateRoom roomID mRoomName mPrivate mCustomData) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "name"        .= mRoomName
       , "private"     .= mPrivate
       , "custom_data" .= mCustomData
       ]
    }
  fromResponse resp = withStatus 204 resp $ pure UpdateRoomResponse

-- | PUT /rooms/_roomID/users/add
data UpdateAddUsers = UpdateAddUsers
  { _roomID  :: RoomID
  , _userIDs :: [UserID]
  }
  deriving Show
-- | Response to UpdateAddUsers
data UpdateAddUsersResponse = UpdateAddUsersResponse
  deriving Show
instance IsRequest UpdateAddUsers UpdateAddUsersResponse where
  toRequest (UpdateAddUsers roomID userIDs) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/users/add"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "user_ids" .= userIDs
       ]
    }
  fromResponse resp = withStatus 204 resp $ pure UpdateAddUsersResponse

-- | PUT /rooms/_roomID/users/remove
data UpdateRemoveUsers = UpdateRemoveUsers
  { _roomID  :: RoomID
  , _userIDs :: [UserID]
  }
  deriving Show
-- | Response to UpdateRemoveUsers
data UpdateRemoveUsersResponse = UpdateRemoveUsersResponse
  deriving Show
instance IsRequest UpdateRemoveUsers UpdateRemoveUsersResponse where
  toRequest (UpdateRemoveUsers roomID userIDs) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/users/remove"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
        [ "user_ids" .= userIDs
        ]
    }
  fromResponse resp = withStatus 204 resp $ pure UpdateRemoveUsersResponse

-- | SUBSCRIBE /rooms/_roomID
data SubscribeRoom = SubscribeRoom
  { _roomID       :: RoomID
  , _messageLimit :: Maybe Limit
  }
  deriving Show
instance IsSubscribe SubscribeRoom RoomEvent where
  toSubscribeRequest (SubscribeRoom roomID mLimit) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> (maybe "" (\limit -> "?message_limit=" <> (Text.pack . show $ limit)) mLimit)
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

-- | SUBSCRIBE /rooms/_roomID/memberships
data SubscribeRoomMemberships = SubscribeRoomMemberships
  { _roomID    :: RoomID
  }
  deriving Show
instance IsSubscribe SubscribeRoomMemberships RoomMembershipEvent where
  toSubscribeRequest (SubscribeRoomMemberships roomID) = SubscriptionRequest
    { _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/memberships"
    }
  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

-- Typing indicators

-- | POST /rooms/_roomID/typing_indicators
data CreateTypingIndicators = CreateTypingIndicators
  { _roomID :: RoomID
  }
  deriving Show
-- | Response to CreateTypingIndicators
data CreateTypingIndicatorsResponse = CreateTypingIndicatorsResponse
  deriving Show
instance IsRequest CreateTypingIndicators CreateTypingIndicatorsResponse where
  toRequest (CreateTypingIndicators roomID) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/typing_indicators"
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure CreateTypingIndicatorsResponse

-- Messages

-- | POST /rooms/_roomID/messages
data CreateMessage = CreateMessage
  { _roomID :: RoomID
  , _parts  :: [MessagePart]
  }
  deriving Show
-- | Response to CreateMessage
data CreateMessageResponse = CreateMessageResponse
  { _messageID :: MessageID
  }
  deriving Show
instance IsRequest CreateMessage CreateMessageResponse where
  toRequest (CreateMessage roomID parts) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/messages"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "parts" .= parts
       ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ withObject "Message" (\o -> CreateMessageResponse <$> o .: "message_id") body

-- | POST /rooms/_roomID/attachments
data CreateAttachment = CreateAttachment
  { _roomID         :: RoomID
  , _contentType    :: ContentType
  , _contentLength  :: ContentLength
  , _name           :: Maybe AttachmentName
  , _customData     :: Maybe CustomData
  }
  deriving Show
-- | Response to CreateAttachment
data CreateAttachmentResponse = CreateAttachmentResponse
  { _attatchmentID :: AttachmentID
  , _uploadURL     :: URL -- ^ URL where attachment can be PUT.
  }
  deriving Show
instance IsRequest CreateAttachment CreateAttachmentResponse where
  toRequest (CreateAttachment roomID contentType contentLength mName mCustomData) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/attachments"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "content_type"    .= contentType
       , "content_length"  .= contentLength
       , "name"            .= mName
       , "custom_data"     .= mCustomData
       ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "Attachment" (\o -> CreateAttachmentResponse <$> o .: "attachment_id" <*> o .: "upload_url") body

-- | GET /rooms/_roomID/messages/_messageID
data GetRoomMessage = GetRoomMessage
  { _roomID    :: RoomID
  , _messageID :: MessageID
  }
  deriving Show
data GetRoomMessageResponse = GetRoomMessageResponse
  { _message :: Message
  }
  deriving Show
instance IsRequest GetRoomMessage GetRoomMessageResponse where
  toRequest (GetRoomMessage roomID messageID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/messages/" <> (Text.pack $ show messageID)
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRoomMessageResponse <$> parseJSON body

-- | GET /rooms/_roomID/messages
data GetRoomMessages = GetRoomMessages
  { _roomID           :: RoomID
  , _initialMessageID :: Maybe MessageID
  , _limit            :: Maybe Limit
  , _messageDirection :: Maybe MessageDirection
  }
  deriving Show
-- | Response to GetRoomMessages
data GetRoomMessagesResponse = GetRoomMessagesResponse
  { _messages :: [Message]
  }
  deriving Show
instance IsRequest GetRoomMessages GetRoomMessagesResponse where
  toRequest (GetRoomMessages roomID mInitialMessageID mLimit mMessageDirection) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/messages" <> queryString
    , _body    = Nothing
    }
    where
      queryString :: Text.Text
      queryString = foldr (\(k,v) acc -> acc <> "&" <> k <> "=" <> v) "?" . catMaybes $
                      maybe Nothing (Just . ("initial_id",) . Text.pack . show) mInitialMessageID
                    : maybe Nothing (Just . ("limit",) . Text.pack . show) mLimit
                    : maybe Nothing (Just . ("direction",) . (\d -> case d of
                                                                      NewestFirst -> "newer"
                                                                      OldestFirst -> "older"
                                                             )) mMessageDirection
                    : []
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRoomMessagesResponse <$> parseJSON body

-- | PUT /rooms/:room_id/messages/:message_id
data EditRoomMessage = EditRoomMessage
  { _roomID       :: RoomID
  , _messageID    :: MessageID
  , _messageParts :: [MessagePart]
  }
  deriving Show
data EditRoomMessageResponse = EditRoomMessageResponse
  deriving Show
instance IsRequest EditRoomMessage EditRoomMessageResponse where
  toRequest (EditRoomMessage roomID messageID parts) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/rooms/" <> roomID <> "/messages/" <> (Text.pack . show $ messageID)
    , _body    = Just . toStrict . encodingToLazyByteString. pairs . mconcat $
        [ "parts" .= parts
        ]
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 204 resp $ pure EditRoomMessageResponse

-- | DELETE /messages/_messageID
data DeleteMessage = DeleteMessage
  { _messageID :: MessageID
  }
  deriving Show
-- | Response to DeleteMessage
data DeleteMessageResponse = DeleteMessageResponse
  deriving Show
instance IsRequest DeleteMessage DeleteMessageResponse where
  toRequest (DeleteMessage messageID) = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit"
    , _version = "v4"
    , _path    = "/messages/" <> (Text.pack . show $ messageID)
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteMessageResponse

