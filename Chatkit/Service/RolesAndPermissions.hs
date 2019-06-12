{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}
{-|
Module      : Chatkit.Service.RolesAndPermissions
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports data structures which model request-responses and subscription-events to the 'roles-and-permissions' Chatkit API as defined by https://pusher.com/docs/chatkit/reference/roles-and-permissions
|-}
module Chatkit.Service.RolesAndPermissions
  (
  -- * Create a Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#creating-a-role
    CreateRole(..)
  , CreateRoleResponse(..)

  -- * Get Roles
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#get-roles
  , GetRoles(..)
  , GetRolesResponse(..)

  -- * Delete a Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#deleting-a-role
  , DeleteRole(..)
  , DeleteRoleResponse(..)

  -- * Update a Users Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#setting-a-user-role
  , UpdateUserRole(..)
  , UpdateUserRoleResponse(..)

  -- * Get a Users Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#get-user-roles
  , GetUserRoles(..)
  , GetUserRolesResponse(..)

  -- * Delete a Users Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#delete-user-role
  , DeleteUserRole(..)
  , DeleteUserRoleResponse(..)

  -- * Get the permissions associated with a Role
  -- | See: https://pusher.com/docs/chatkit/reference/roles-and-permissions#get-role-permissions
  , GetRolePermissions(..)
  , GetRolePermissionsResponse(..)

  -- * Update a Roles permissions
  , UpdateRolePermissions(..)
  , UpdateRolePermissionsResponse(..)
  )
  where

import Chatkit
import Chatkit.Model

import Pusher
import Pusher.Client
import Pusher.Client.Request

import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8
import qualified Data.Text as Text

-- | POST /roles
data CreateRole = CreateRole
  { _role :: Role
  }
  deriving Show
-- | Response to CreateRole
data CreateRoleResponse = CreateRoleResponse
  deriving Show
instance IsRequest CreateRole CreateRoleResponse where
  toRequest (CreateRole role) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/roles"
    , _body    = Just . toStrict . encodingToLazyByteString . toEncoding $ role
    }
  fromResponse resp = withStatus 201 resp $ pure CreateRoleResponse

-- | GET /roles
data GetRoles = GetRoles
  deriving Show
-- | Response to GetRoles
data GetRolesResponse = GetRolesResponse
  { _roles :: [Role]
  }
  deriving Show
instance IsRequest GetRoles GetRolesResponse where
  toRequest _ = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/roles"
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRolesResponse <$> parseJSONList body

-- | DELETE /roles/_roleName/scope/_scopeType
data DeleteRole = DeleteRole
  { _roleName  :: RoleName
  , _scope     :: RoleScope
  }
  deriving Show
-- | Response to DeleteRole
data DeleteRoleResponse = DeleteRoleResponse
  deriving Show
instance IsRequest DeleteRole DeleteRoleResponse where
  toRequest (DeleteRole roleName scope)  = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/roles/" <> roleName <> "/scope/" <> case scope of
         GlobalScope -> "global"
         RoomScope   -> "room"
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteRoleResponse

-- User Roles

-- | PUT /users/_userID/roles
data UpdateUserRole = UpdateUserRole
  { _userID   :: UserID
  , _roleName :: RoleName
  , _roomID   :: Maybe RoomID
  }
  deriving Show
-- | Response to UpdateUserRole
data UpdateUserRoleResponse = UpdateUserRoleResponse
  deriving Show
instance IsRequest UpdateUserRole UpdateUserRoleResponse where
  toRequest (UpdateUserRole userID roleName mRoomID) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/users/" <> userID <> "/roles"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "name"    .= roleName
       , "room_id" .= mRoomID
       ]
    }
  fromResponse resp = withStatus 201 resp $ pure $ UpdateUserRoleResponse

-- | GET /users/_userID/roles
data GetUserRoles = GetUserRoles
  { _userID :: UserID
  }
  deriving Show
-- | Response to GetUserRoles
data GetUserRolesResponse = GetUserRolesResponse
  { _roles :: [Role]
  }
  deriving Show
instance IsRequest GetUserRoles GetUserRolesResponse where
  toRequest (GetUserRoles userID) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/users/" <> userID <> "/roles"
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetUserRolesResponse <$> parseJSONList body

-- | DELETE /users/_userID/roles
data DeleteUserRole = DeleteUserRole
  { _userID :: UserID
  , _roomID :: Maybe RoomID
  }
  deriving Show
-- | Response to DeleteUserRole
data DeleteUserRoleResponse = DeleteUserRoleResponse
  deriving Show
instance IsRequest DeleteUserRole DeleteUserRoleResponse where
  toRequest (DeleteUserRole userID mRoomID)  = Request
    { _method  = "DELETE"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/users/" <> userID <> "/roles" <> maybe "" ("?room="<>) mRoomID
    , _body    = Nothing
    }
  fromResponse resp = withStatus 204 resp $ pure DeleteUserRoleResponse

-- Role permissions

-- | GET /roles/_roleName/scope/_scopeName/permissions
data GetRolePermissions = GetRolePermissions
  { _roleName :: RoleName
  , _scope    :: RoleScope
  }
  deriving Show
-- | Response to GetRolePermissions
data GetRolePermissionsResponse = GetRolePermissionsResponse
  { _rolePermissionsResponse :: [RolePermission]
  }
  deriving Show
instance IsRequest GetRolePermissions GetRolePermissionsResponse where
  toRequest (GetRolePermissions roleName roleScope) = Request
    { _method  = "GET"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/roles/" <> roleName <> "/scope/" <> (case roleScope of
                   GlobalScope -> "global"
                   RoomScope   -> "room"
                 ) <> "/permissions"
    , _body    = Nothing
    }
  fromResponse resp@(Response _headers body _mHeaders) = withStatus 200 resp $ GetRolePermissionsResponse <$> parseJSONList body

-- | PUT /roles/_roleName/scope/_scopeName/permission
data UpdateRolePermissions = UpdateRolePermissions
  { _roleName          :: RoleName
  , _scopeName         :: RoleScope

  -- At least one key required.
  , _addPermissions    :: [RolePermission]
  , _removePermissions :: [RolePermission]
  }
  deriving Show
-- | Response to UpdateRolePermissions
data UpdateRolePermissionsResponse = UpdateRolePermissionsResponse
  deriving Show
instance IsRequest UpdateRolePermissions UpdateRolePermissionsResponse where
  toRequest (UpdateRolePermissions roleName scopeName addPermissions removePermissions) = Request
    { _method  = "PUT"
    , _headers = []
    , _service = "chatkit_authorizer"
    , _version = "v2"
    , _path    = "/roles/" <> roleName <> "/scope/" <> (case scopeName of
                   GlobalScope -> "global"
                   RoomScope   -> "room"
                 ) <> "/permissions"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
       [ "add_permissions"    .= addPermissions
       , "remove_permissions" .= removePermissions
       ]
    }
  fromResponse resp = withStatus 204 resp $ pure UpdateRolePermissionsResponse

