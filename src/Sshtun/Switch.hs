-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Switch
   ( switchWatcher
   )
   where

import Control.Concurrent.STM
import Network.Curl
import System.Process

import Sshtun.Common
import Sshtun.Conf


switchWatcher :: ConfMap -> TVar Shared -> IO ()
switchWatcher conf shared = do
   flagUrl <- confString "switchUrl" conf
   body <- curlGetString flagUrl []
   switch shared $ bodyToState body

   putStrLn "switchWatcher starting to wait now"
   confInt "switchPollInterval" conf >>= sleep
   putStrLn "switchWatcher done waiting"
   switchWatcher conf shared


bodyToState :: (CurlCode, String) -> DesiredState
bodyToState (CurlOK, "1\n") = Run
bodyToState _               = Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   putStrLn "switch setting Run now"
   atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (tst, Run)

switch shared Stop = do
   putStrLn "switch setting Stop now"
   tst <- atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (Stopped, Stop)
      return tst
   stop tst
            

stop :: TunnelState -> IO ()
stop (Running ph) = terminateProcess ph
stop Stopped      = return ()
