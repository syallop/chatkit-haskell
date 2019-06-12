{-# LANGUAGE
    DuplicateRecordFields
  , OverloadedStrings
  #-}
{-|
Module      : Main
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This executable tests the Chatkit HTTP2 Client by issuing (at least) one of each
type of request/ subscription to each Chatkit service.

These tests are pretty fragile. They cannot easily be re-ordered due to tests
depending on the state of previous tests. This should be seen less of a
comprehensive test suite and more of an example program showing how to call each
api.
|-}
module Main where

import Data.Map (Map)
import Data.Time.Clock.POSIX
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System
import qualified Data.ByteString.UTF8 as BS
import Control.Monad.IO.Class                         (liftIO)
import Control.Concurrent
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus as P

import Chatkit
import Pusher
import Pusher.Model
import qualified Chatkit.Test.Unit as Unit

-- Evil unsafe IO because it was fast to copy.
{-# NOINLINE metrics #-}
metrics :: P.Vector (Text,Text) P.Counter
metrics = P.unsafeRegister
        $ P.vector ("test_name"::Text,"pass"::Text)
        $ P.counter
        $ P.Info "test" "Whether an individual test case has passed"

-- | This program attempts to test a happy path through every Chatkit endpoint.
main :: IO ()
main = do
  -- Read envars to determine where to test
  instanceID <- Text.pack     <$> System.getEnv "instance"
  key        <- Text.pack     <$> System.getEnv "key"
  secretKey  <- BS.fromString <$> System.getEnv "secret"
  cluster    <- Text.pack     <$> System.getEnv "cluster"
  mesh       <- BS.fromString <$> System.getEnv "mesh"
  repeat     <- Text.pack     <$> System.getEnv "repeat"
  let clusterName = case cluster of
        "us1"
          -> US1
        unknownCluster
          -> CustomCluster unknownCluster
  let host = PusherPlatform
  Just env <- mkPusherEnv instanceID key clusterName host []

  -- Setup a metrics server to host test results
  let port = 19090
  print $ "Starting prometheus metrics on port " <> show port
  _ <- P.register P.ghcMetrics
  _thread <- forkIO $ run 19090 $ P.prometheus P.def emptyServer

  -- If we've been asked to repeat the tests, do so forever.
  let unitMetrics = Just $ Unit.Metrics $ metrics
  case repeat of
    "true" -> foreverTest secretKey env unitMetrics
    _      -> runTest     secretKey env unitMetrics

-- Run a single iteration of the unit tests
runTest :: KeySecret -> PusherEnv -> Maybe Unit.Metrics -> IO ()
runTest keySecret env metrics = do
  _ <- runPusher env $ Unit.test keySecret metrics
  pure ()

-- Run the unit tests forever
foreverTest :: KeySecret -> PusherEnv -> Maybe Unit.Metrics -> IO ()
foreverTest keySecret env metrics = do
  print $ "Running tests"
  runTest keySecret env metrics
  print $ "Pausing before re-running tests."
  threadDelay 50000
  foreverTest keySecret env metrics

-- a server with no endpoints
emptyServer :: Wai.Application
emptyServer request respond = do
  response <- case Wai.pathInfo request of
    _ -> return $ Wai.responseLBS status200 [(hContentType, "text/html")] "404"
  respond response

