{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Pusher.Client.Token
import Chatkit
import Chatkit.Service.Core

import Data.Text             (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

clusterName   = US1
host          = PusherPlatform
commonHeaders = []

myInstanceID = "instance-id-here"
myKeyID      = "key-id-here"
myKeySecret  = "key-secret-here"

-- External IDs can be provided when creating users.
-- For example, you may already have a notion of ids where the admin is id:0
myAdminId :: Text
myAdminId = "0"

-- The name as visible to other users
myAdminName :: Text
myAdminName = "admin"

-- Create a token with super-user permissions for the admin id.
createAdminAccessToken :: Pusher AccessToken
createAdminAccessToken = do
  issuedAt <- pusherIO getPOSIXTime
  let expiresAt = issuedAt + 60*60
  let subject   = myAdminId
  let claims    = Map.fromList [("su", JSON.Bool True)]
  case mkAccessToken myInstanceID myKeyID myKeySecret issuedAt expiresAt subject claims of
    Nothing
      -> pusherFail "Could not create admin token for id"
    Just adminToken
      -> pure adminToken

-- Create an admin user that can interact with Chatkit rooms, etc.
createAdminUser :: AccessToken -> Pusher User
createAdminUser accessToken = do
  CreateUserResponse adminUser <- pusherRequest (Just accessToken) $ CreateUser myAdminId myAdminName Nothing Nothing
  pure adminUser

-- A collection of initial users that we'd like to add to Chatkit.
--
-- This could be the result of a "SELECT userid FROM users" query against your
-- application.
getInitialUserIdsToImport :: IO [Text]
getInitialUserIdsToImport = pure ["alice", "bob"]

-- Initialise a collection of user ids as Chatkit users.
initialiseUsers :: AccessToken -> [Text] -> Pusher [User]
initialiseUsers token userids = do
  -- Add existing users to Chatkit in a batch
  CreateUsersResponse users <- pusherRequest (Just token) . CreateUsers . map (\userId -> CreateUser ("id-"<>userId) userId Nothing Nothing) $ userids
  pure users

-- Create a new room with two users as members.
createOneOnOneRoom :: AccessToken -> User -> User -> Pusher Room
createOneOnOneRoom token (User user1 _ _ _ _) (User user2 _ _ _ _) = do
  -- Create a new Room containing two users
  let roomID = user1 <> "-" <> user2
  CreateRoomResponse room <- pusherRequest (Just token) $ CreateRoom
    { _roomID     = Just roomID
    , _roomName   = Just roomID
    , _private    = Just True
    , _customData = Nothing
    , _userIDs    = Just [user1, user2]
    }
  pure room

-- Send a welcome message to a room from the user specified in the token.
sendWelcomeMessageToRoom :: AccessToken -> Room -> Pusher ()
sendWelcomeMessageToRoom token rooms = do
  pusherRequest (Just token) . CreateMessage "roomid" $ [TextPart "Welcome!" "text/plain"]
  pure ()

main :: IO ()
main = do
  -- The Chatkit environment specifies how to talk to your instance
  Just env <- mkPusherEnv myInstanceID myKeyID clusterName host commonHeaders

  result <- runPusher env $ do
    -- Create an admin user to represent our server. This user will be able to
    -- perform super-user actions against the Chatkit api (such as adding users
    -- to rooms, creating users, etc).
    adminAccessToken <- createAdminAccessToken
    adminUser        <- createAdminUser adminAccessToken

    -- Add our existing users to Chatkit, initialising them with empty Avatars
    -- and other details.
    ourUserIDs <- pusherIO getInitialUserIdsToImport
    users      <- initialiseUsers adminAccessToken ourUserIDs

    -- Add each user to a room with the admin
    rooms <- mapM (createOneOnOneRoom adminAccessToken adminUser) users

    -- Greet each user from the admin
    mapM_ (sendWelcomeMessageToRoom adminAccessToken) rooms

    pure ()

  print result

