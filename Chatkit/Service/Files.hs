{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Chatkit.Service.Files
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'files' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/files-api
-}
module Chatkit.Service.Files
  (
  -- * Retrieve a single File
  -- | See: https://pusher.com/docs/chatkit/reference/files-api#get-file
    GetFile(..)
  , GetFileResponse(..)

  -- * Create a new File
  -- | See: https://pusher.com/docs/chatkit/reference/files-api#post-file
  , CreateFile(..)
  , CreateFileResponse(..)

  -- * Delete a File
  -- | See: https://pusher.com/docs/chatkit/reference/files-api#delete-file
  , DeleteFile(..)
  , DeleteFileResponse(..)

  -- * Delete a Users Files
  -- | See: https://pusher.com/docs/chatkit/reference/files-api#delete-user-files
  , DeleteUsersFiles(..)
  , DeleteUsersFilesResponse(..)
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
import Data.Text.Encoding
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as Text

-- | GET /rooms/_roomID/files/_fileName
data GetFile = GetFile
  { _roomID   :: RoomID
  , _fileName :: FileName
  }
  deriving Show
-- | Response to GetFile
data GetFileResponse = GetFileResponse
  { _file         :: File
  , _resourceLink :: URL
  , _timeToLive   :: Int
  }
  deriving Show
instance IsRequest GetFile GetFileResponse where
  toRequest (GetFile roomID fileName) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_files"
    , _version = "v1"
    , _path    = "/rooms/" <> roomID <> "/files/" <> fileName
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "File" (\o -> GetFileResponse <$> o .: "file" <*> o .: "resource_link" <*> o .: "ttl") body

-- | POST /rooms/_roomID/users/_userID/files/_fileName
data CreateFile = CreateFile
  { _roomID     :: RoomID
  , _userID     :: UserID
  , _fileName   :: FileName
  , _fileLength :: ContentLength
  , _fileBytes  :: ByteString
  }
  deriving Show
-- | Response to CreateFile
data CreateFileResponse = CreateFileResponse
  { _resourceLink :: URL
  , _fileType     :: FileType
  }
  deriving Show
instance IsRequest CreateFile CreateFileResponse where
  toRequest (CreateFile roomID userID fileName fileLength fileBytes) = Request
    { _method  = "POST"
    , _headers = [("Content-Type"  ,"multipart/form-data; boundary="<>toStrict boundary)
                 ,("Content-Length", BS.fromString . show . BS.length $ body)
                 ]
    , _service = "chatkit_files"
    , _version = "v1"
    , _path    = "/rooms/" <> roomID <> "/users/" <> userID <> "/files/" <> fileName
    , _body    = Just body
    }
    where
      boundary = "BoUnDaRyThAtShOuLdNoTApPeArInThEfIlE"
      body = toStrict . mconcat $
        [ "--"<>boundary<>"\r\n"
        , "Content-Disposition: form-data; name=\"file\"; filename=\""<>(fromStrict . encodeUtf8 $ fileName)<>"\"\r\n"
        , "Content-Type: text/plain\r\n\r\n"
        , fileBytes
         , "\r\n\r\n--"<>boundary<>"--"
        ]

  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ withObject "File" (\o -> CreateFileResponse <$> o .: "resource_link" <*> o .: "type") body

-- | DELETE /rooms/_roomID/files/_fileName
data DeleteFile = DeleteFile
  { _roomID   :: RoomID
  , _fileName :: FileName
  }
  deriving Show
-- | Response to DeleteFile
data DeleteFileResponse = DeleteFileResponse
  deriving Show
instance IsRequest DeleteFile DeleteFileResponse where
  toRequest (DeleteFile roomID fileName) = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit_files"
    , _version = "v1"
    , _path    = "/rooms/" <> roomID <> "/files/" <> fileName
    , _body    = Nothing
    }
  fromResponse resp = withStatus 200 resp $ pure DeleteFileResponse

-- | DELETE /users/_userID
data DeleteUsersFiles = DeleteUsersFiles
  { _userID :: UserID
  }
  deriving Show
-- | Response to DeleteUsersFiles
data DeleteUsersFilesResponse = DeleteUsersFilesResponse
  deriving Show
instance IsRequest DeleteUsersFiles DeleteUsersFilesResponse where
  toRequest (DeleteUsersFiles userID) = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit_files"
    , _version = "v1"
    , _path    = "/users/" <> userID
    , _body    = Nothing
    }
  fromResponse resp = withStatus 200 resp $ pure DeleteUsersFilesResponse

