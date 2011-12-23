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
import Sshtun.Log


switchWatcher :: ConfMap -> TVar Shared -> IO ()
switchWatcher conf shared = do
   flagUrl <- confString "switchUrl" conf
   body <- curlGetString flagUrl []
   switch shared $ bodyToState body

   logM INFO "switchWatcher starting to wait now"
   confInt "switchPollInterval" conf >>= sleep
   logM INFO "switchWatcher done waiting"
   switchWatcher conf shared


bodyToState :: (CurlCode, String) -> DesiredState
bodyToState (CurlOK, "1\n") = Run
bodyToState _               = Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   logM INFO "switch setting Run now"
   atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (tst, Run)

switch shared Stop = do
   logM INFO "switch setting Stop now"
   tst <- atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (Stopped, Stop)
      return tst
   stop tst
            

stop :: TunnelState -> IO ()
stop (Running ph) = terminateProcess ph
stop Stopped      = return ()
