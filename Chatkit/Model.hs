{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , EmptyDataDeriving
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
{-|
Module      : Chatkit.Model
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

Defines shared data types used in making Chatkit requests/ subscriptions and
parsing their responses/ events.
|-}

module Chatkit.Model where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.ByteString
import Data.Scientific
import Data.Text
import Data.Time.Clock
import qualified Data.Text as Text

-- Below: Lots of type aliases which can/ should be made stronger.

-- | A timestamp describing when an event happened
type Timestamp = UTCTime

-- | A fully qualified URL
type URL = Text

-- | Name associated with a User
type UserName = Text

-- | Unique ID associated with a User
type UserID = Text

-- | A positive limit used to restrict the number of responses to a request.
type Limit = Int

-- | Users can be associated with arbitrary custom data
type CustomData = Object

-- | Unique ID associated with a Message
type MessageID = Int

-- | Unique ID associated with a Room
type RoomID = Text

-- | Unique ID associated with an Attachment
type AttachmentID = Text

-- | Name associated with an Attatchment
type AttachmentName = Text

-- | Size in Bytes of an Attatchment
type ContentLength = Int

-- | Describes the type of content in an Attatchment
type ContentType = Text

-- | The MIMEType of some data
type MIMEType = Text

-- | Events from a User subscription.
data UserEvent
  -- | Rooms a User is a member of at the time of opening the subscription
  = InitialUserState
    { _timestamp   :: Timestamp
    , _rooms       :: [UserRoom]
    , _currentUser :: User
    }

  -- | The User has been added to a Room.
  | AddedToRoom
    { _timestamp :: Timestamp
    , _room      :: UserRoom
    }

  -- | The User has been removed from a Room.
  | RemovedFromRoom
    { _timestamp :: Timestamp
    , _roomID    :: RoomID
    }

  -- | The Room has been updated.
  | RoomUpdated
    { _timestamp :: Timestamp
    , _room      :: UserRoom
    }

  -- | The Room has been deleted.
  | RoomDeleted
    { _timestamp :: Timestamp
    , _roomID    :: RoomID
    }

  -- | Undocumented. A new Cursor is created?
  | NewCursor
    { _timestamp :: Timestamp
    , _cursor    :: Cursor
    }
  deriving Show
instance FromJSON UserEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"

            case eventName of
              "initial_state"
                -> InitialUserState
                   <$> pure timestamp
                   <*> dat .: "rooms"
                   <*> dat .: "current_user"

              "added_to_room"
                -> AddedToRoom
                   <$> pure timestamp
                   <*> dat .: "room"

              "removed_from_room"
                -> RemovedFromRoom
                   <$> pure timestamp
                   <*> dat .: "room_id"

              "room_updated"
                -> RoomUpdated
                   <$> pure timestamp
                   <*> dat .: "room"

              "room_deleted"
                -> RoomDeleted
                   <$> pure timestamp
                   <*> dat .: "room_id"

              "new_cursor"
                -> NewCursor
                   <$> pure timestamp
                   <*> parseJSON (Object dat)
              unknown
                -> fail $ "Unknown User event: " <> show unknown

    _ -> typeMismatch "UserEvent" v

-- | Events from a Room subscription includes Messages and Typing Indicators.
data RoomEvent
  -- | A new Message has been sent to the Room
  = NewMessage
    { _timestamp :: Timestamp
    , _message   :: Message
    }

  -- | A User is currently typing.
  | IsTyping
    { _timestamp :: Timestamp
    , _userID    :: UserID
    }
  deriving Show
instance FromJSON RoomEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"

            case eventName of
              "new_message"
                -> NewMessage
                   <$> pure timestamp
                   <*> parseJSON (Object dat)

              "is_typing"
                -> IsTyping
                   <$> pure timestamp
                   <*> dat .: "user_id"

              unknown
                -> fail $ "Unknown Room event: " <> show unknown
    _ -> typeMismatch "RoomEvent" v

-- | Events from a Room membership subscription include joining and leaving events.
data RoomMembershipEvent
  -- | The Users who belong to the Room at the time of opening the subscription.
  = InitialRoomMembership
    { _timestamp :: Timestamp
    , _userIDs   :: [UserID]
    }

  -- | A new User has joined the Room.
  | UserJoined
    { _timestamp :: Timestamp
    , _userID    :: UserID
    }

  -- | A User has left the Room.
  | UserLeft
    { _timestamp :: Timestamp
    , _userID    :: UserID
    }
  deriving Show
instance FromJSON RoomMembershipEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp         <- o .: "timestamp"
            dat               <- o .: "data"

            case eventName of
              "initial_state"
                -> InitialRoomMembership
                     <$> pure timestamp
                     <*> dat .: "user_ids"

              "user_joined"
                -> UserJoined
                     <$> pure timestamp
                     <*> dat .: "user_id"

              "user_left"
                -> UserLeft
                     <$> pure timestamp
                     <*> dat .: "user_id"
              unknown
                -> fail $ "Unknown Room membership event: " <> show unknown
    _ -> typeMismatch "RoomMembershipEvent" v

-- | Events from a User Cursor subscription.
data UserCursorEvent
  -- | The Users previously set Cursors at the time of opening the Subscription
  = InitialUserCursors
    { _timestamp :: Timestamp
    , _cursors   :: [Cursor]
    }

  -- | The Users Cursor has been created or updated.
  | NewUserCursor
    { _timestamp :: Timestamp
    , _cursor    :: Cursor
    }
  deriving Show
instance FromJSON UserCursorEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"

            case eventName of
              "initial_state"
                -> InitialUserCursors
                   <$> pure timestamp
                   <*> dat .: "cursors"

              "new_cursor"
                -> NewUserCursor
                   <$> pure timestamp
                   <*> parseJSON (Object dat)

              unknown
                -> fail $ "Unknown User Cursor event: " <> show unknown
    _ -> typeMismatch "UserCursorEvent" v

-- | Events from a Room Cursor subscription.
data RoomCursorEvent
  -- | The Rooms previously set Cursors for Users at the time of opening the
  -- subscription.
  = InitialRoomCursors
    { _timestamp :: Timestamp
    , _cursors   :: [Cursor]
    }

  -- | A Room Cursor has been created or updated.
  | NewRoomCursor
    { _timestamp :: Timestamp
    , _cursor    :: Cursor
    }
  deriving Show
-- TODO: This is the same instance as UserCursorEvent
instance FromJSON RoomCursorEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"

            case eventName of
              "initial_state"
                -> InitialRoomCursors
                   <$> pure timestamp
                   <*> dat .: "cursors"

              "new_cursor"
                -> NewRoomCursor
                   <$> pure timestamp
                   <*> parseJSON (Object dat)

              unknown
                -> fail $ "Unknown Room Cursor event: " <> show unknown
    _ -> typeMismatch "RoomCursorEvent" v

-- | Registering as Online does not currently return any events.
data RegisterOnlineEvent
  deriving Show
instance FromJSON RegisterOnlineEvent where
  parseJSON = typeMismatch "RegisterOnlineEvent"

-- | Events for a Users Presence status subscription.
data UserPresenceEvent
  -- | The User is now online.
  = Online
    { _timestamp :: Timestamp
    }

  -- | The User is now offline.
  | Offline
    { _timestamp :: Timestamp
    }
  deriving Show

instance FromJSON UserPresenceEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"
            case eventName of
              "presence_state"
                -> do state :: Text <- dat .: "state"
                      case state of
                        "online"
                          -> Online  <$> pure timestamp
                        "offline"
                          -> Offline <$> pure timestamp
                        unknown
                          -> fail $ "Unknown User Presence event: " <> show unknown
    _ -> typeMismatch "UserPresenceEvent" v

-- | Name associated with a Room
type RoomName = Text

-- | Positive position of a Cursor
type CursorPosition = Int

-- | Read Cursors are currently the only Cursor type.
data CursorType
  = ReadCursor
  deriving Show
instance FromJSON CursorType where
  parseJSON v = case v of
    Number s -> case toBoundedInteger s of
      Nothing -> typeMismatch "CursorType" v
      Just (i :: Int) -> case i of
        0 -> pure ReadCursor
        _ -> typeMismatch "CursorType" v
    _          -> typeMismatch "CursorType" v

-- | Name of a file
type FileName = Text

-- | Type of a File
data FileType
  = AudioType
  | VideoType
  | ImageType
  | FileType
  deriving Show
instance FromJSON FileType where
  parseJSON v = case v of
    String s -> case s of
      "audio" -> pure AudioType
      "video" -> pure VideoType
      "image" -> pure ImageType
      "file"  -> pure FileType
      _       -> typeMismatch "FileType" v
    _ -> typeMismatch "FileType" v

-- | A Role is either scoped globally or to a single Room.
data RoleScope
  = GlobalScope
  | RoomScope
  deriving Show
instance ToJSON RoleScope where
  toJSON = undefined
  toEncoding GlobalScope = text "global"
  toEncoding RoomScope   = text "room"
instance FromJSON RoleScope where
  parseJSON v = case v of
    String s -> case s of
      "global" -> pure GlobalScope
      "room"   -> pure RoomScope
      _        -> typeMismatch "RoleScope" v
    _ -> typeMismatch "RoleScope" v

-- | Name associated with a Role.
type RoleName = Text

-- | Permissions that may be associated with a Role. Some permissions are only
-- relevant at the global scope.
data RolePermission
  = RoomJoin                  -- ^ room:join
  | RoomLeave                 -- ^ room:leave
  | RoomMembersAdd            -- ^ room:members:add
  | RoomMembersRemove         -- ^ room:members:remove
  | RoomCreate                -- ^ room:create
  | RoomDelete                -- ^ room:delete
  | RoomUpdate                -- ^ room:update
  | RoomMessagesGet           -- ^ room:messages:get
  | MessageCreate             -- ^ message:create
  | RoomTypingIndicatorCreate -- ^ room:typing_indicator:create
  | PresenceSubscribe         -- ^ presence:subscribe
  | UserUpdate                -- ^ user:update
  | UserGet                   -- ^ user:get
  | RoomGet                   -- ^ room:get
  | UserRoomsGet              -- ^ user:rooms:get
  | CursorsReadGet            -- ^ cursors:read:get
  | CursorsReadSet            -- ^ cursors:read:set
  | FileGet                   -- ^ file:get
  | FileCreate                -- ^ file:create
  deriving Show
instance ToJSON RolePermission where
  toJSON = undefined
  toEncoding rolePermission = text $ case rolePermission of
    RoomJoin
      -> "room:join"
    RoomLeave
      -> "room:leave"
    RoomMembersAdd
      -> "room:members:add"
    RoomMembersRemove
      -> "room:members:remove"
    RoomCreate
      -> "room:create"
    RoomDelete
      -> "room:delete"
    RoomUpdate
      -> "room:update"
    RoomMessagesGet
      -> "room:messages:get"
    MessageCreate
      -> "message:create"
    RoomTypingIndicatorCreate
      -> "room:typing_indicator:create"
    PresenceSubscribe
      -> "presence:subscribe"
    UserUpdate
      -> "user:update"
    UserGet
      -> "user:get"
    RoomGet
      -> "room:get"
    UserRoomsGet
      -> "user:rooms:get"
    CursorsReadGet
      -> "cursors:read:get"
    CursorsReadSet
      -> "cursors:read:set"
    FileGet
      -> "file:get"
    FileCreate
      -> "file:create"
instance FromJSON RolePermission where
  parseJSON v = case v of
    String s -> case s of
      "room:join"
        -> pure RoomJoin
      "room:leave"
        -> pure RoomLeave
      "room:members:add"
        -> pure RoomMembersAdd
      "room:members:remove"
        -> pure RoomMembersRemove
      "room:create"
        -> pure RoomCreate
      "room:delete"
        -> pure RoomDelete
      "room:update"
        -> pure RoomUpdate
      "room:messages:get"
        -> pure RoomMessagesGet
      "message:create"
        -> pure MessageCreate
      "room:typing_indicator:create"
        -> pure RoomTypingIndicatorCreate
      "presence:subscribe"
        -> pure PresenceSubscribe
      "user:update"
        -> pure UserUpdate
      "user:get"
        -> pure UserGet
      "room:get"
        -> pure RoomGet
      "user:rooms:get"
        -> pure UserRoomsGet
      "cursors:read:get"
        -> pure CursorsReadGet
      "cursors:read:set"
        -> pure CursorsReadSet
      "file:get"
        -> pure FileGet
      "file:create"
        -> pure FileCreate
      _ -> fail . Text.unpack $ s <> " is not a recognised RolePermission"
    _ -> typeMismatch "RolePermission" v

-- | A Message is sent to a room and is made up of many message-parts.
data Message = Message
  { _messageID     :: MessageID
  , _sendersUserID :: UserID
  , _sentToRoomID  :: RoomID
  , _createdAt     :: Timestamp
  , _updatedAt     :: Timestamp
  , _parts         :: [MessagePart]
  }
  deriving Show
instance FromJSON Message where
  parseJSON v = case v of
    Object o -> Message <$> o .: "id"
                        <*> o .: "user_id"
                        <*> o .: "room_id"
                        <*> o .: "created_at"
                        <*> o .: "updated_at"
                        <*> o .: "parts"
    _ -> typeMismatch "Message" v

-- | A Message part is either Text, a URL or an Attachment.
data MessagePart
  = TextPart Text MIMEType
  | URLPart URL MIMEType
  | AttachmentPart Attachment MIMEType
  deriving Show
instance ToJSON MessagePart where
  toJSON = undefined
  toEncoding messagePart = case messagePart of
    TextPart txt mimeType
      -> pairs . mconcat $
           [ "type"    .= mimeType
           , "content" .= txt
           ]

    URLPart url mimeType
      -> pairs . mconcat $
           [ "type" .= mimeType
           , "url"  .= url
           ]

    AttachmentPart attachment mimeType
      -> pairs . mconcat $
           [ "type"       .= mimeType
           , "attachment" .= attachment
           ]
instance FromJSON MessagePart where
  parseJSON v = case v of
    Object o -> textPart o <|> urlPart o <|> attachmentPart o
    _ -> typeMismatch "MessagePart" v
    where
      textPart o = TextPart <$> o .: "content" <*> o .: "type"
      urlPart o = URLPart <$> o .: "url" <*> o .: "type"
      attachmentPart o = AttachmentPart <$> o .: "attachment" <*> o .: "type"

-- | The direction Messages are returned in.
data MessageDirection
  = NewestFirst
  | OldestFirst
  deriving Show

-- | Attachments may be uploaded as a MessagePart.
data Attachment = Attachment
  { _attatchmentID   :: AttachmentID
  , _attatchmentName :: AttachmentName
  , _downloadURL     :: URL
  , _refreshURL      :: URL
  , _expiration      :: Timestamp
  , _sizeBytes       :: ContentLength
  , _customData      :: Maybe CustomData
  }
  deriving Show
instance ToJSON Attachment where
  toJSON = undefined
  toEncoding (Attachment id name downloadURL refreshURL expiration size mCustomData) = pairs . mconcat $
    [ "id" .= id
    , "download_url" .= downloadURL
    , "refresh_url"  .= refreshURL
    , "expiration"   .= expiration
    , "name"         .= name
    , "size"         .= size
    , "custom_data"  .= mCustomData
    ]
instance FromJSON Attachment where
  parseJSON v = case v of
    Object o -> Attachment <$> o .: "id"
                           <*> o .: "name"
                           <*> o .: "download_url"
                           <*> o .: "refresh_url"
                           <*> o .: "expiration"
                           <*> o .: "size"
                           <*> o .: "custom_data"
    _ -> typeMismatch "Attachment" v

-- | A known User
data User = User
  { _userID    :: UserID
  , _userName  :: UserName
  , _avatarURL :: Maybe URL
  , _createdAt :: Timestamp
  , _updatedAt :: Timestamp
  }
  deriving Show
instance FromJSON User where
  parseJSON v = case v of
    Object o -> User <$> o .: "id"
                     <*> o .: "name"
                     <*> o .:? "avatar_url"
                     <*> o .: "created_at"
                     <*> o .: "updated_at"
    _ -> typeMismatch "User" v

-- | A known Room
data Room = Room
  { _roomID      :: RoomID
  , _createdByID :: UserID
  , _roomName    :: RoomName
  , _customData  :: Maybe CustomData
  , _private     :: Bool
  , _createdAt   :: Timestamp
  , _updatedAt   :: Timestamp
  }
  deriving Show
instance FromJSON Room where
  parseJSON v = case v of
    Object o -> Room <$> o .:  "id"
                     <*> o .:  "created_by_id"
                     <*> o .:  "name"
                     <*> o .:? "custom_data"
                     <*> o .:  "private"
                     <*> o .:  "created_at"
                     <*> o .:  "updated_at"
    _ -> typeMismatch "Room" v

-- | A Room that has just been joined has a set of known current members.
data JoinedRoom = JoinedRoom
  { _room          :: Room
  , _memberUserIDs :: [UserID]
  }
  deriving Show
instance FromJSON JoinedRoom where
  parseJSON v = JoinedRoom <$> parseJSON v
                           <*> (case v of
                                 Object o
                                   -> o .: "member_user_ids"
                               )

-- | A Room from the point of view of a User.
data UserRoom = UserRoom
  { _room          :: Room
  , _unreadCount   :: Int
  , _lastMessageAt :: Maybe Timestamp
  }
  deriving Show
instance FromJSON UserRoom where
  parseJSON v = case v of
    Object o -> UserRoom <$> parseJSON v <*> o .: "unread_count" <*> o .:? "last_message_at"
    _        -> typeMismatch "UserRoom" v

-- | A Cursor denotes a points to a Message in a Room.
data Cursor = Cursor
  { _type       :: CursorType
  , _roomID     :: RoomID
  , _userID     :: UserID
  , _position   :: CursorPosition
  , _updatedAt  :: Timestamp
  }
  deriving Show
instance FromJSON Cursor where
  parseJSON v = case v of
    Object o -> Cursor <$> o .: "cursor_type" -- This is expected to always be 0. The other fields presumably depend upon this type.
                       <*> o .: "room_id"
                       <*> o .: "user_id"
                       <*> o .: "position"
                       <*> o .: "updated_at"
    _ -> typeMismatch "Cursor" v

-- | A File is named opaque bytes.
data File = File
  { _fileName      :: FileName
  , _fileSizeBytes :: ContentLength
  , _lastModified  :: Int
  }
  deriving Show
instance FromJSON File where
  parseJSON v = case v of
    Object o -> File <$> o .: "name" <*> o .: "bytes" <*> o .: "last_modified"
    _ -> typeMismatch "File" v

-- | A named Role is associated with a set of permissions at a Global or Room
-- scope.
data Role = Role
  { _scope       :: RoleScope
  , _name        :: RoleName
  , _permissions :: [RolePermission]
  }
  deriving Show
instance ToJSON Role where
  toJSON = undefined
  toEncoding (Role scope name permissions) = pairs . mconcat $
    [ "scope"       .= scope
    , "name"        .= name
    , "permissions" .= permissions
    ]
instance FromJSON Role where
  parseJSON v = case v of
    Object o -> Role <$> o .: "scope"
                     <*> (o .: "name" <|> o .: "role_name")
                     <*> o .: "permissions"
    _ -> typeMismatch "Role" v

-- | Unique ID associated with a job
type JobID = Text

-- | The current status of a job
data JobStatus
  -- ^ The job is still in progress. In this state the resource cannot be updated, but some remnants, such as messages, may still be visible
  = InProgress

  -- ^ The job is completed, and no trace of the resource remains.
  | Completed

  -- ^ An internal failure caused the deletion of the resource to fail. You may repost the job for re-processing.
  | Failed
  deriving Show
instance ToJSON JobStatus where
  toJSON = undefined
  toEncoding j = text $ case j of
    InProgress
      -> "in_progress"
    Completed
      -> "completed"
    Failed
      -> "failed"
instance FromJSON JobStatus where
  parseJSON v = case v of
    String s -> case s of
      "in_progress"
        -> pure InProgress
      "completed"
        -> pure Completed
      "failed"
        -> pure Failed
    _ -> typeMismatch "JobStatus" v

