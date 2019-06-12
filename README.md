# Chatkit-Haskell

[Chatkit](https://pusher.com/chatkit) is a hosted chat API that allows you to add one-to-one and group chat to your app, along with typing indicators, file attachments and storage, user online presence and a flexible permissions system.

This respository provides Haskell bindings to the Chatkit API that can be consumed by the [pusher-platform-haskell](https://github.com/syallop/Pusher-Platform-Haskell) client. It is not a server or client SDK (but could be used to build one).

Note: This package is unofficial and alpha quality.

Continue reading this document to learn how to [install this dependency](#Installing),
[connect to an instance](#Connecting-to-an-instance), [make requests](#Requests),
[establish subscriptions](#Subscriptions) and [handle errors](#Errors). Or
consult [documentation](#Documentation).

**Contents:**
- [Examples](#Examples) 
- [Installing](#Installing)
- [Connecting to an instance](#Connecting-to-an-instance)
  - [AccessTokens](#AccessTokens)
    - [QuickStart](#Quick-start-development)
    - [Fairly QuickStart](#Still-pretty-quick-start-server-side-development)
- [Using](#Using)
  - [Requests](#Requests)
  - [Subscriptions](#Subscriptions)
  - [Errors](#Errors)
    - [ErrorResponses](#ErrorResponses)
    - [Failures](#Failures)
    - [Short-circuiting](#Short-circuiting)
- [Documentation](#Documentation)
- [Developing this library](#Developing-this-library)

## Examples
There are self-contained examples under `/Examples` which can be built with
`stack build` from within this repository.

The [Server example](Example/Server/Server.hs) shows how to create access tokens, users, rooms and send
messages from the server. 

## Installing

1. Start a project

If you have an Existing application, skip to step 2 otherwise start a new
Haskell project. [Stack](https://docs.haskellstack.org/en/stable/README/) is
known to work but you should be able to use other tools such as cabal.

With stack:

```bash
mkdir MyApp && cd MyApp && stack init
```

2. Add `chatkit-haskell` as a dependency to `MyApp.Cabal` file:
```
  ...
  build-depends:       base >= 4.12
                     , chatkit-haskell
  ...
```

3. Tell your build tool where to find this dependency

`chatkit-haskell` is not currently on Hackage or in a stack resolver.

If using stack add this repository as a source to your `stack.yaml`:
```
resolver: lts-14.20
packages:
- '.'
extra-deps:
- git: https://github.com/syallop/Chatkit-Haskell.git
  commit: master
- git: https://github.com/syallop/Pusher-Platform-Haskell.git
  commit: master
```

If using cabal directly you may install this repository globally:
```sh
git clone https://github.com/syallop/Chatkit-Haskell.git && cd Chatkit-Haskell && cabal install
```

## Connecting to an instance

To interact with Chatkit you will need your instance and key. Obtain credentials
or create a free instance in the [dashboard](https://dash.pusher.com).

The "Instance Locator" displayed in the dashboard takes the form `VERSION:CLUSTER:INSTANCEID`.
The "Secret Key" takes the form `KEYID:KEYSECRET`.

Note:
- The `keySecret` is the private key used to sign requests. It should NOT be
  shared with untrusted clients.
- The `instanceID` identifies your instance and can be shared
- The `keyID` identifies the secret key you will use to authorize requests and
  can be shared.

Create an environment that points at a single instance of Chatkit:

```haskell
import Pusher

instanceID  = "my-instance-id"
keyID       = "my-key-id"
keySecret   = error "Only supply key secret to trusted servers"
clusterName = US1
host        = PusherPlatform

main :: IO ()
main = do
  Just env <- mkPusherEnv instanceID keyID clusterName host []
  ...
```

Use this environment to issue requests and establish subscriptions by using
`runPusher` like:

```haskell
main :: IO
main = do
  Just env <- mkPusherEnv instanceID keyID clusterName host []

  result <- runPusher env pusherActions 
  case result of
    PusherSuccess ()
      -> putStrLn "Successfully executed actions"

    PusherErrorResponse errResp
      -> putStrLn $ "Got error response from api: " <> show errResp

    PusherFailure errMsg
      -> fail $ show errMsg

pusherActions :: Pusher ()
pusherActions = do
  pusherIO $ putStrLn "Hello world"
  -- More Pusher actions can be chained here. Failures will shortcircuit.
```

### AccessTokens

Most requests will require an `AccessToken` to be supplied for authorization. These are created
by signing a [JWT](https://tools.ietf.org/html/rfc7519) with your `SecretKey`.

`SecretKeys` must never be given to untrusted clients as it would allow them to
authorize any request.

See the [Authentication Flow docs](https://pusher.com/docs/chatkit/authentication) for more
details.

If you are in an untrusted client context you cannot securely generate
`AccessTokens`. You should communicate with a trusted server context who is
responsible for deciding whether to grant you an appropriate `AccessToken`.

#### Quick start development
You may enable the "Test Token Provider" in the dashboard. Request
`AccessTokens` like:

```haskell
import Chatkit.Service.InsecureTokenProvider

getTokenInsecurelyFor :: UserId -> Pusher AccessToken
getTokenInsecurelyFor userId = do
  response <- pusherRequest (CreateToken userId) Nothing 
  pure $ _accessToken response 
```

**Warning**: It is important to disable this option for a production Chatkit instance as the
endpoint is unauthenticated meaning everybody has admin access to your instance.

#### Still-pretty-quick-start server-side development 

If you are in a trusted server context, you may generate `AccessToken`s:
```haskell
import Data.Map.Strict
import Data.Text
import Data.Time.Clock.POSIX
import Test.RandomStrings
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

import Pusher.Client.Token

pusherActions :: Pusher ()
pusherActions = do
  pusherIO $ putStrLn "Hello world"
  -- More Pusher actions can be chained here. Failures will shortcircuit.

  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  pure ()

createAccessTokenAtTrustedServer :: Text -> IO (Maybe AccessToken)
createAccessTokenAtTrustedServer forUser = do
  issuedAt <- getPOSIXTime
  let expiresAt = issuedAt + 60*60
  let subject = forUser
  let claims = Map.fromList [("su", JSON.Bool True)]

  pure $ mkAccessToken instanceID keyID keySecret issuedAt expiresAt subject claims

generateUserID :: Text -> IO Text
generateUserID name = do
  suffix <- randomWord randomASCII 10
  pure $ name <> "-" <> pack suffix
```

**Warning**: This token has no restrictions on permissions across the entire
instance. 

## Using

All API calls documented in the [API docs](https://pusher.com/docs/chatkit/reference/latest) have definitions under similarly named modules. 

| Feature                                                         | API docs                                                        | Chatkit-Haskell Module                                                        |
| ----                                                            | ----                                                            | ----                                                                          |
| Core (Rooms, Users, Messages)                                   | https://pusher.com/docs/chatkit/reference/latest                | [Chatkit.Service.Core](Chatkit/Service/Core.hs)                               |
| Roles & Permissions (Roles, User Roles, Permissions)            | https://pusher.com/docs/chatkit/reference/roles-and-permissions | [Chatkit.Service.RolesAndPermissions](Chatkit/Service/RolesAndPermissions.hs) |
| Scheduler (Asynchronous delete Users/ Rooms)                    | https://pusher.com/docs/chatkit/reference/scheduler             | [Chatkit.Service.Scheduler](Chatkit/Service/Scheduler.hs)                     |
| Presence (Setting and subscribing to online state)              | https://pusher.com/docs/chatkit/reference/presence              | [Chatkit.Service.Presence](Chatkit/Service/Presence.hs)                       |
| Read Cursors (Setting, getting and subscribing to read cursors) | https://pusher.com/docs/chatkit/reference/cursors               | [Chatkit.Service.Cursors](Chatkit/Service/Cursors.hs)                         |
| Files (Upload, get and delete Files)                            | https://pusher.com/docs/chatkit/reference/files-api             | [Chatkit.Service.Files](Chatkit/Service/Files.hs)                             |

### Requests

Requests and Responses are first-class data structures that define:
- How to talk to the Chatkit api
- How to serialise and deserialise request and response bodies
- How to interpolate query parameters

Requests can be fed to a `pusherRequest` function to produce a matching
response type in the `Pusher` context.

For example the [create user](https://pusher.com/docs/chatkit/reference/latest)
endpoint is found under `Chatkit.Service.Core`. It's request type is defined
like:

```haskell
-- | POST /users
data CreateUser = CreateUser
  { _userID     :: UserID
  , _userName   :: UserName
  , _avatarURL  :: Maybe URL
  , _customData :: Maybe CustomData
  }
```

It's response type looks like:

```haskell
-- | Response to CreateUser
data CreateUserResponse = CreateUserResponse
  { _user :: User
  }
```

This can be passed to `pusherRequest` to return a `CreateUserResponse`.

### Subscriptions

Subscriptions are primarily used for client-side development and deliver events
as they occur in real time. Examples include subscribing to:
- Messages sent to a room
- Changes in read cursors in a room
- Changes in users online state

The Subscription format is documented in the [api docs](https://pusher.com/docs/chatkit/reference/subscription-events) however this library intends to abstract most of the details.

Each subscription defined in the api-docs has a corresponding `Subscribe*`
data-structure which defines how to open the subscription as well as a `*Event`
data-structure which defines the type of events the subscription may return.

E.G. To print all new messages sent to a room: 
```haskell
printRoomMessages :: RoomId -> Chatkit ()
printRoomMessages roomID = do
  roomSubscription <- pusherSubscription (SubscribeRoom roomID Nothing) (Just token)
  printRoomMessages' roomSubscription
  where
    printRoomMessages' :: Subscription RoomEvent -> Pusher ()
    printRoomMessages' roomSubscription = do
      ev <- pusherReadEvent roomSubscription timeout 
      putStrLn $ case ev of
        Right (NewMessage _timestamp message)
          -> "Received message: " <> show message 
        _ -> "" 
      printRoomMessages' roomSubscription
```

If a client stops caring for a subscription, they _should_ call `close` on it.

## Errors
Executing the `Pusher` type with `runPusher` returns a `PusherResult`.
Asides from success, the result may contain two classes of errors -
`PusherFailures` and `PusherErrorResponses`.

### ErrorResponses
`PusherErrorResponse`s indicate an 'expected' error returned by the Chatkit
API. The contained `ErrorResponse` will have a status code, a description of
the error and a link to documentation.

The type looks like:
```haskell
data ErrorResponse = ErrorResponse
  { -- ^ The status code mirrors HTTP. I.E. 4xx indicates the client made a bad
    -- request. 5xx indicates the server encountered some internal error.
    _errorStatusCode   :: Int
  , _errorResponseBody :: ErrorResponseBody
  }

-- | An Error 'successfully' returned from the API.
-- The status code mirrors http - 4xx indicates the client made a bad request,
-- 5xx indicates the server etc.
data ErrorResponseBody = ErrorResponseBody
  {
    -- ^ A unique string identifying the specific type of error.
    _errorType        :: Text

    -- ^ A longer description of the meaning of the error.
  , _errorDescription :: Maybe Text

    -- ^ A link to further documentation on the error.
  , _errorURI         :: Text

    -- ^ Key-value pairs that are specific to the error type and may provide more
    -- detail as to what caused the error.
  , _errorAttributes  :: Maybe (Map Text Value)
  }
```

The status codes and presence of a `Retry-After` header indicate whether a
request can be retried.

E.G.

- 2xx could indicate a subscription has unexpected closed 'successfully' from
  the servers point of view. You may want to re-establish.
- 429 with `Retry-After` header indicates you have hit a ratelimit and should
  retry after the specified period.
- 4xx indicates the request is malformed in some way and likely cannot be
  retried without modifying some property indicated by the error type.
- 5xx indicates the server is having an internal problem. A `Retry-After` header
  should be present and indicate how soon you should retry the request.

### Failures
`PusherFailure`s indicate a logic error in the libraries implementation or it's
dependencies which should be reported. You may be able to retry these requests
but it is more likely the library is in an invalid state and should be
completely re-initialised.

For example, this could be caused by:
- An internal error in the underlying HTTP2 client
- A double close on a `Subscription`

### Short-circuiting
By default, both types of error short-circuit a `Pusher` computation. I.E. in a
`do` block, if one chatkit request fails, it's error will be immediately
returned and any remaining requests will not be issued. If you do NOT want this
behavior, `pusherTry` can be used to explicitly handle `ErrorResponses`.

## Documentation

A local copy of the reference docs can be built and opened with:
```bash
stack haddock --open
```
Note: This will build documentation for every transitive dependency and may take a long time on first run.

## Developing this library

Build with:
```bash
stack build
```

Supplied suitable envars, a test-suite can be ran against a Chatkit instance:
```
stack test
```

