{-# LANGUAGE
    DuplicateRecordFields
  , OverloadedStrings
  , RankNTypes
  #-}
{-|
Module      : Chatkit.Test.Unit
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This library exports tests for the Chatkit HTTP2 Client which issue (at least)
one of each type of request/ subscription to each Chatkit service.

These tests are pretty fragile. They cannot easily be re-ordered due to tests
depending on the state of previous tests. This should currently be seen less of a
comprehensive test suite and more of an example program showing how to call each
api.
|-}
module Chatkit.Test.Unit
  ( test
  , Metrics (..)
  )
  where

import Pusher.Client
import Pusher.Client.Request
import Pusher.Client.Error
import Pusher.Client.Subscribe
import Pusher.Client.Token
import Pusher.Client.HTTP2
import Chatkit.Model
import Pusher.Model

import Chatkit
import Chatkit.Service.Core
import Chatkit.Service.Cursors
import Chatkit.Service.Files
import Chatkit.Service.InsecureTokenProvider
import Chatkit.Service.Presence
import Chatkit.Service.Scheduler
import Chatkit.Service.RolesAndPermissions
import Chatkit.Service.Undocumented

import Data.Map (Map)
import Data.Time.Clock.POSIX
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Environment as System
import qualified Data.ByteString.UTF8 as BS
import Control.Concurrent

import qualified Prometheus as P
import System.Timeout

data Metrics = Metrics
  { testCounter :: P.Vector (Text,Text) P.Counter
  }

-- | This program attempts to test a happy path through every Chatkit endpoint.
test :: KeySecret -> Maybe Metrics -> Pusher ()
test keySecret metrics = do

  time <- pusherIO getPOSIXTime
  let newUser = Text.pack $ "user" <> show time

  -- Generate a SU token for creating users, etc and some convenience functions
  -- to create requests/ subscriptions using this token.
  suToken <- testChatkit "new_su_token" metrics $ suToken keySecret newUser
  let suRequest :: forall req resp. IsRequest req resp => req -> Pusher resp
      suRequest = pusherRequest (Just suToken)

      suSubscribe :: forall req event. IsSubscribe req event => req -> Pusher (Subscription event)
      suSubscribe = pusherSubscribe (Just suToken)

  -- Delete all resources in the instance to ensure a clean slate
  _ <- testChatkit "delete_all_resources" metrics $ suRequest DeleteResources

  -- Create a single user
  CreateUserResponse (User userID _ _ _ _) <- testChatkit "create_user" metrics $ suRequest $ CreateUser newUser newUser Nothing Nothing

  -- Open a Presence Subscription
  userPresenceSubscription <- testChatkit "subscribe_presence" metrics $ suSubscribe $ SubscribeUserPresence userID

  -- Register User as being online
  userOnlineSubscription <- testChatkit "register_user_online" metrics $ suSubscribe $ SubscribeRegisterOnline userID

  -- Expect notification that the user is OFFLINE
  -- TODO: Use metrics
  pusherIO . Text.putStrLn $ "Expect User Offline Event"
  _userOnlineEv <- pusherTry $ expectEvent userPresenceSubscription (\ev -> case ev of
    Offline _timestamp
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected a User Offline event but got: " <> (Text.pack $ show ev)
            return False
    )
  -- Expect notification that the user is online
  pusherIO . Text.putStrLn $ "Expect User Online Event"
  _userOnlineEv <- pusherTry $ expectEvent userPresenceSubscription (\ev -> case ev of
    Online _timestamp
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected a User Online event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Open a Subscription to all User Events
  pusherIO . Text.putStrLn $ "Subscribe to users"
  usersSubscription <- suSubscribe SubscribeUsers
  initialEvent <- pusherReadEvent (Just 2000000) usersSubscription
  pusherIO . Text.putStrLn $ "Event from users subscription: " <> (Text.pack $ show initialEvent)

  -- Open a Subscription to the Users Cursors
  userCursorsSubscription <- testChatkit "subscribe_user_cursors" metrics $ suSubscribe $ SubscribeUserCursors newUser
  -- TODO: Use metrics
  _initialEvent <- pusherTry $ expectEvent userCursorsSubscription (\ev -> case ev of
    InitialUserCursors _timestamp cursors
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected an Initial User Cursors event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Create a token for a user
  CreateTokenResponse accessToken userID _expiresIn <- testChatkit "create_token" metrics $ suRequest $ CreateToken newUser

  -- Create multiple users in batch
  CreateUsersResponse newUsers <- testChatkit "create_users" metrics $ suRequest $ CreateUsers [CreateUser (newUser<>"0") (newUser<>"0") Nothing Nothing, CreateUser (newUser<>"1") (newUser<>"1") Nothing Nothing]

  -- Get a single user
  GetUserResponse gotUser <- testChatkit "get_user" metrics $ suRequest $ GetUser newUser

  -- Get multiple users
  GetUsersResponse gotUsers <- testChatkit "get_users" metrics $ suRequest $ GetUsers Nothing (Just 10)

  -- Get multiple users by id
  GetUsersByIDsResponse gotUsersByID <- testChatkit "get_users_by_ids" metrics $ suRequest $ GetUsersByIDs [newUser<>"0",newUser<>"1"]

  -- Update user
  testChatkit "update_user" metrics $ suRequest $ UpdateUser newUser (Just $ newUser <> "updated") Nothing Nothing

  -- Create a Role
  let adminRole = "roleadmin" <> (Text.pack . show $ time)
  let role = Role
        { _scope       = GlobalScope
        , _name        = adminRole
        , _permissions =
             [ RoomJoin
             , RoomLeave
             , RoomMembersAdd
             , RoomMembersRemove
             , RoomCreate
             , RoomDelete
             , RoomUpdate
             , MessageCreate
             , RoomTypingIndicatorCreate
             , PresenceSubscribe
             , UserUpdate
             , UserGet
             , RoomGet
             , UserRoomsGet
             , CursorsReadGet
             , CursorsReadSet
             , FileGet
             , FileCreate
             ]
        }
  testChatkit "create_role" metrics $ suRequest (CreateRole role)

  -- Get all Roles
  GetRolesResponse roles <- testChatkit "get_roles" metrics $ suRequest GetRoles

  -- Update a Users Role.
  -- - add an admin role with all permissions
  testChatkit "update_user_role" metrics $ suRequest $ UpdateUserRole newUser adminRole Nothing

  -- Get a Users Roles
  GetUserRolesResponse roles <- testChatkit "get_user_roles" metrics $ suRequest $ GetUserRoles newUser

  -- Get a single Roles Permissions
  GetRolePermissionsResponse permissions <- testChatkit "get_role_permissions" metrics $ suRequest $ GetRolePermissions adminRole GlobalScope

  -- Create a new room
  let roomName = "roomname" <> (Text.pack . show $ time)
  CreateRoomResponse (Room roomID _ _ _ _ _ _) <- testChatkit "create_room" metrics $ suRequest $ CreateRoom Nothing (Just roomName) (Just False) Nothing Nothing

  -- Create a useless room role and assign it to the user
  let roomRole@(Role uselessScope uselessName []) = Role
        { _scope       = RoomScope
        , _name        = "roleroom" <> (Text.pack . show $ time)
        , _permissions = []
        }
  testChatkit "create_role" metrics $ suRequest $ CreateRole roomRole
  testChatkit "update_user_role" metrics $ suRequest $ UpdateUserRole newUser uselessName (Just roomID)

  -- Update a Roles permissions
  testChatkit "update_role_permissions" metrics $ suRequest $ UpdateRolePermissions uselessName uselessScope [CursorsReadGet] []

  -- Delete a Users Role
  testChatkit "delete_user_role" metrics $ suRequest $ DeleteUserRole newUser (Just uselessName)

  -- Delete a Role
  testChatkit "delete_role" metrics $ suRequest $ DeleteRole uselessName uselessScope

  -- Subscribe to Room Cursors
  roomCursorsSubscription <- testChatkit "subscribe_room_cursors" metrics $ suSubscribe $ SubscribeRoomCursors roomID
  -- TODO: Use metrics
  _initialEvent <- pusherTry $ expectEvent roomCursorsSubscription (\ev -> case ev of
    InitialRoomCursors _timestamp cursors
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected an Initial Room Cursors event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Subscribe to Room membership
  roomMembershipSubscription <- testChatkit "subscribe_room_memberships" metrics $ suSubscribe $ SubscribeRoomMemberships roomID
  -- TODO: Use metrics
  _initialEvent <- pusherTry $ expectEvent roomMembershipSubscription (\ev -> case ev of
    InitialRoomMembership _timestamp users
      | users == [userID]
        -> return True

      | otherwise
        -> do pusherIO . Text.putStrLn $ "Expected InitialRoomMembership event to include the creating user " <> (Text.pack $ show userID) <> " but got: " <> (Text.pack $ show users)
              return False

    _ -> do pusherIO . Text.putStrLn $ "Expected an InitialRoomMembership event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Get A Room
  GetRoomResponse (Room newRoomID _ _ _ _ _ _) <- testChatkit "get_room" metrics $ suRequest $ GetRoom roomID

  -- User joins a Room
  CreateJoinRoomResponse joinedRoom <- testChatkit "create_join_room" metrics $ suRequest $ CreateJoinRoom newUser newRoomID

  -- Expect an Event for the User joining a Room on the user subscription
  -- - Two are recieved because the user created the room and then joined it.
  -- TODO: Use metrics
  pusherIO . Text.putStrLn $ "Expect a User joined Room event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    AddedToRoom _timestamp room
      | ((_roomID :: Room -> RoomID) . (_room :: UserRoom -> Room) $ room) == ((_roomID :: Room -> RoomID) . (_room :: JoinedRoom -> Room) $ joinedRoom)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got user joined Room event for incorrect room. Got" <> (Text.pack $ show room) <> " expected " <> (Text.pack $ show joinedRoom)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a joined Room event but got: " <> (Text.pack $ show ev)
            return False
    )
  pusherIO . Text.putStrLn $ "Expect a User joined Room event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    AddedToRoom _timestamp room
      | ((_roomID :: Room -> RoomID) . (_room :: UserRoom -> Room) $ room) == ((_roomID :: Room -> RoomID) . (_room :: JoinedRoom -> Room) $ joinedRoom)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got user joined Room event for incorrect room. Got" <> (Text.pack $ show room) <> " expected " <> (Text.pack $ show joinedRoom)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a joined Room event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Expect an Event for the User joining the Room on the room subscription
  pusherIO . Text.putStrLn $ "Expect a UserJoined event"
  _ev <- pusherTry $ expectEvent roomMembershipSubscription (\ev -> case ev of
    UserJoined _timestamp joiningUserID
      | joiningUserID == userID
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got user joined event for incorrect user. Got " <> (Text.pack $ show joiningUserID) <> " expected " <> (Text.pack $ show userID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a user joined event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Subscribe to a Room
  roomSubscription <- testChatkit "subscribe_room" metrics $ suSubscribe $ SubscribeRoom roomID Nothing

  -- Get a users rooms
  GetUserRoomsResponse userRooms <- testChatkit "get_user_rooms" metrics $ suRequest $ GetUserRooms (newUser<>"0") Nothing

  -- Get rooms
  GetRoomsResponse rooms <- testChatkit "get_rooms" metrics $ suRequest $ GetRooms Nothing (Just True)

  -- Update Room
  testChatkit "update_room" metrics $ suRequest $ UpdateRoom roomID (Just "newroomname") Nothing Nothing

  -- Expect an Event for the Users Room being updated
  -- TODO: Use metrics.
  pusherIO . Text.putStrLn $ "Expect a Room Updated event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    RoomUpdated _timestamp userRoom
      | roomID  == ((_roomID :: Room -> RoomID) . (_room :: UserRoom -> Room) $ userRoom)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got Room Updated Room event for incorrect room. Got" <> (Text.pack $ show userRoom) <> " expected " <> (Text.pack $ show roomID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a Room Updated event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Do something with typing indicators
  testChatkit "create_typing_indicators" metrics $ suRequest $ CreateTypingIndicators roomID

  -- Expect an Event for the Room typing indicator?
  -- TODO: use metrics.
  pusherIO . Text.putStrLn $ "Expect a typing indicator on the room"
  _ev <- pusherTry $ expectEvent roomSubscription (\ev -> case ev of
    IsTyping _timestamp typingUserID
      | typingUserID == userID
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got IsTyping event for unexpected user. Got: " <> (Text.pack $ show typingUserID) <> " expected: " <> (Text.pack $ show userID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a IsTyping event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Send a Message
  let messageParts = [TextPart "message" "text/plain"]
  CreateMessageResponse messageID <- testChatkit "create_message" metrics $ suRequest $ CreateMessage roomID messageParts

  -- Retrieve the Message
  GetRoomMessageResponse message <- testChatkit "get_room_message" metrics $ suRequest $ GetRoomMessage roomID messageID

  -- Edit a Message
  let editedMessageParts = [TextPart "edited-message" "text/plain"]
  _ <- testChatkit "edit_room_message" metrics $ suRequest $ EditRoomMessage roomID messageID editedMessageParts

  -- Expect a New Message Event for the Room
  -- TODO: Use metrics
  pusherIO . Text.putStrLn $ "Expect a New Message event on the room"
  _ev <- pusherTry $ expectEvent roomSubscription (\ev -> case ev of
    NewMessage _timestamp message
      | (_sendersUserID message) == userID
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got NewMessage event from unexpected user. Got: " <> (Text.pack $ show (_sendersUserID message)) <> " expected: " <> (Text.pack $ show userID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a NewMessage event but got: " <> (Text.pack $ show ev)
            return False
    )
  -- Expect a RoomUpdated Event indicating the unread count has increased
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    RoomUpdated _timestamp userRoom
      |    (roomID == ((_roomID :: Room -> RoomID) . (_room :: UserRoom -> Room) $ userRoom))
        && (_unreadCount userRoom == 1)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got an unexpected unreadcount,room. Got: " <> (Text.pack $ show userRoom) <> " expected " <> (Text.pack $ show (userID,"1"))
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a Room Updated event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- User leaves a Room
  let leaveRoomID = newRoomID
  testChatkit "create_leave_room" metrics $ suRequest $ CreateLeaveRoom newUser leaveRoomID
  -- Expect an Event for the User leaving a Room
  -- TODO: Use metrics
  pusherIO . Text.putStrLn $ "Expect a User removed from Room event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    RemovedFromRoom _timestamp roomID
      | roomID  == leaveRoomID
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got user removed Room event for incorrect room. Got: " <> (Text.pack $ show roomID) <> " expected " <> (Text.pack $ show leaveRoomID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a removed Room event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Add Users to a Room
  testChatkit "update_add_users" metrics $ suRequest $ UpdateAddUsers roomID [newUser<>"0",newUser<>"1"]

  -- Send an Attachment
  CreateAttachmentResponse attachmentID attachmentURL <- testChatkit "create_attachment" metrics $ suRequest $ CreateAttachment roomID "text/plain" 1 Nothing Nothing

  -- Get Room Messages
  GetRoomMessagesResponse messages <- testChatkit "get_room_messages" metrics $ suRequest $ GetRoomMessages roomID Nothing Nothing Nothing

  -- Update Cursor
  testChatkit "update_cursor" metrics $ suRequest $ UpdateCursor roomID userID 1
  --- Expect a Cursor Update Event on the User subscription
  -- TODO: Use metrics
  pusherIO . Text.putStrLn $ "Expect User Cursor update Event"
  _cursorEv <- pusherTry $ expectEvent userCursorsSubscription (\ev -> case ev of
    NewUserCursor _timestamp cursor
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected a User Cursor event but got: " <> (Text.pack $ show ev)
            return False
    )
  -- Expect a Cursor Update Event on the Room subscription
  pusherIO . Text.putStrLn $ "Expect Room Cursor update Event"
  _cursorEv <- pusherTry $ expectEvent roomCursorsSubscription (\ev -> case ev of
    NewRoomCursor _timestamp cursor
      -> return True
    _ -> do pusherIO . Text.putStrLn $ "Expected a Room Cursor event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Get Cursor
  GetCursorResponse mCursor <- testChatkit "get_cursor" metrics $ suRequest $ GetCursor roomID userID

  -- Get User Cursors
  GetUserCursorsResponse cursors <- testChatkit "get_user_cursors" metrics $ suRequest $ GetUserCursors userID

  -- Get Room Cursors
  GetRoomCursorsResponse cursors <- testChatkit "get_room_cursors" metrics $ suRequest $ GetRoomCursors roomID

  -- Create File
  let fileName = "fileName" -- <> (Text.pack . show $ time)
  CreateFileResponse url fileType <- testChatkit "create_file" metrics $ suRequest $ CreateFile roomID userID fileName 1 "a"

  -- Get File
  GetFileResponse file fileLink fileTTL <- testChatkit "get_file" metrics $ suRequest $ GetFile roomID fileName

  -- Delete File
  testChatkit "delete_file" metrics $ suRequest $ DeleteFile roomID fileName

  -- Delete a Message
  testChatkit "delete_message" metrics $ suRequest $ DeleteMessage messageID

  -- Delete Users Files
  testChatkit "delete_users_files" metrics $ suRequest $ DeleteUsersFiles userID

  -- Remove Users from a Room
  testChatkit "update_remove_users" metrics $ suRequest $ UpdateRemoveUsers roomID [newUser<>"0",newUser<>"1"]

  -- Re-add user to room so we can test the User Delete event.
  CreateJoinRoomResponse joinedRoom <- testChatkit "create_join_room" metrics $ suRequest $ CreateJoinRoom newUser newRoomID
  -- TODO: Use metrics.
  pusherIO . Text.putStrLn $ "Expect a User joined Room event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    AddedToRoom _timestamp room
      | ((_roomID :: Room -> RoomID) . (_room :: UserRoom -> Room) $ room) == ((_roomID :: Room -> RoomID) . (_room :: JoinedRoom -> Room) $ joinedRoom)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got user joined Room event for incorrect room. Got" <> (Text.pack $ show room) <> " expected " <> (Text.pack $ show joinedRoom)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a joined Room event but got: " <> (Text.pack $ show ev)
            return False
    )
  pusherIO . Text.putStrLn $ "Expect a Cursor Room event"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    NewCursor _timestamp (Cursor ReadCursor cursorRoomID cursorUserID position _updatedAt)
      |    (cursorRoomID == ((_roomID :: Room -> RoomID) . (_room :: JoinedRoom -> Room) $ joinedRoom))
        && (cursorUserID == newUser)
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got NewCursor Event for incorrect room,user. Got" <> (Text.pack $ show (cursorRoomID,cursorUserID)) <> " expected " <> (Text.pack $ show (joinedRoom, newUser))
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected a New Cursor event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Delete Room
  testChatkit "delete_room" metrics $ suRequest $ DeleteRoom roomID

  -- Expect a room deleted (user removed first?)
  -- TODO: Use Metrics
  pusherIO . Text.putStrLn $ "Expect a Room Deleted"
  _ev <- pusherTry $ expectEvent usersSubscription (\ev -> case ev of
    RoomDeleted _timestamp deletedRoomID
      | deletedRoomID == roomID
       -> return True

      | otherwise
       -> do pusherIO . Text.putStrLn $ "Got Room Deleted event for incorrect room. Got" <> (Text.pack $ show deletedRoomID) <> " expected " <> (Text.pack $ show roomID)
             return False

    _ -> do pusherIO . Text.putStrLn $ "Expected Room Deleted event but got: " <> (Text.pack $ show ev)
            return False
    )

  -- Create and then asynchronously delete a user
  --let newUserToDelete = newUser <> "ToDelete"
  --Right (CreateUserResponse (User userIDToDelete _ _ _ _)) <- testChatkit "create_user" metrics $ createUser suToken (CreateUser newUserToDelete newUserToDelete Nothing Nothing)
  --expectJobSuccess "delete_user_job" metrics ((\(CreateDeleteUserJobResponse jobID) -> jobID) <$> createDeleteUserJob suToken (CreateDeleteUserJob userIDToDelete) suToken)

  -- Create a new room to test asynchronous deletion
  let roomNameToDelete = roomName <> "ToDelete"
  let roomIDToDelete = "delete-id-" <> (Text.pack . show $ time)
  --pusherIO $ Text.putStrLn roomIDToDelete
  --Right (CreateRoomResponse (Room _ _ _ _ _ _ _)) <- testChatkit "create_room" metrics $ createRoom (CreateRoom (Just roomIDToDelete) (Just roomNameToDelete) suToken (Just False) Nothing Nothing)
  --Right (CreateDeleteRoomJobResponse roomJobID) <- testChatkit "create_delete_room_job" metrics $ createDeleteRoomJob suToken (CreateDeleteRoomJob roomIDToDelete)
  --expectJobSuccess "delete_room_job" metrics ((\(CreateDeleteRoomJobResponse jobID) -> jobID) <$> createDeleteRoomJob suToken (CreateDeleteRoomJob roomIDToDelete) suToken)

  -- Delete the initial suUser
  testChatkit "delete_user" metrics $ suRequest $ DeleteUser newUser

  return ()

-- Generate a super-user token for a user.
suToken :: KeySecret -> UserID -> Pusher AccessToken
suToken keySecret userID = do
  issuedAt <- pusherIO getPOSIXTime
  let expiresAt = issuedAt + 60 * 60
  env <- getEnv
  case mkAccessToken (_instanceID env) (_keyID env) keySecret issuedAt expiresAt userID (Map.fromList [("su",JSON.Bool True)]) of
    Nothing
      -> pusherFail "Failed to create an access token"

    Just accessToken
      -> return accessToken

-- Expect that a Subscriptions next event matches some predicate. If so, return
-- that event.
-- Fails if a timeout is exceeded.
expectEvent :: IsSubscribe req event => Subscription event -> (event -> Pusher Bool) -> Pusher event
expectEvent subscription predicate = do
  let timeout = Just 2000000
  ev <- pusherReadEvent timeout subscription
  case ev of
    Left eos
      -> pusherFail $ "End of stream while expecting an event: " <> (Text.pack . show $ eos)

    Right ev
      -> do pass <- predicate ev
            if pass
              then return ev
              else pusherFail "Event did not pass the given predicate"

-- Execute an action that creates a job, and then poll the job 'a few' times
-- with 'some' delay until it's confirmed to have succeeded (or failed).
expectJobSuccess :: Text -> Maybe Metrics -> Pusher JobID -> AccessToken -> Pusher ()
expectJobSuccess testName metrics createJob token = do
  jobID   <- testChatkit testName metrics createJob
  success <- pollJobSuccess testName metrics jobID token
  testResult metrics testName success ()

-- Poll a job 'a few' times with 'some' delay until the job is confirmed to
-- have succeeded (or failed).
pollJobSuccess :: Text -> Maybe Metrics -> JobID -> AccessToken -> Pusher Bool
pollJobSuccess jobName metrics jobID accessToken = pollJobSuccess' few
  where
    few = 10

    pollJobSuccess' 0 = pusherFail $ "Polled job " <> (Text.pack . show $ few) <> " times without success"
    pollJobSuccess' n = do
      GetJobStatusResponse _jobID jobStatus <- testChatkit ("poll_" <> (Text.pack . show $ n ) <> "_" <> jobName) metrics $ pusherRequest (Just accessToken) (GetJobStatus jobID)
      case jobStatus of
          InProgress
           -> do pusherIO $ threadDelay 250000
                 pollJobSuccess' (n - 1)

          Completed
           -> pure True

          Failed
           -> pure False

-- Output a test success/ failure and increment metrics if used.
testResult :: Show a => Maybe Metrics -> Text -> Bool -> a -> Pusher ()
testResult mMetrics testName pass resp = do
  pusherIO $ Text.putStrLn $ mconcat
    [ testName
    , if pass then " passed" else "FAILED"
    , "\n"
    , Text.pack $ show resp
    , "\n"
    ]

  case mMetrics of
    Nothing -> pure ()
    Just (Metrics testCounter)
      -> pusherIO $ P.withLabel testCounter (testName, Text.pack . show $ pass) $ P.incCounter

-- Announce the test to be ran, run it and record results.
testChatkit :: Show resp => Text -> Maybe Metrics -> Pusher resp -> Pusher resp
testChatkit testName metrics testFunction = do
  pusherIO . Text.putStrLn $ testName
  mResp <- pusherTry testFunction
  case mResp of
    PusherFailure errMsg
      -> do testResult metrics testName False errMsg
            pusherFail errMsg

    PusherErrorResponse errResp
      -> do testResult metrics testName False errResp
            pusherErrorResponse errResp

    PusherSuccess resp
      -> do testResult metrics testName True resp
            return resp

