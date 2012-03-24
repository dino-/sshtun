-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Switch
   ( switchWatcher
   )
   where

import Control.Concurrent.STM
import Network.Curl

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log


switchWatcher :: Conf -> TVar Shared -> IO ()
switchWatcher conf shared = do
   body <- curlGetString (switchUrl conf) []
   switch shared $ bodyToState body

   logM INFO "Switch watcher starting to wait"
   sleep $ switchPollInterval conf
   logM INFO "Switch watcher done waiting"
   switchWatcher conf shared


bodyToState :: (CurlCode, String) -> DesiredState
bodyToState (CurlOK, "1\n") = Run
bodyToState _               = Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   logM INFO "Switch watcher setting desired state to Run"
   run shared

switch shared Stop = do
   logM INFO "Switch watcher setting desired state to Stop"
   stop shared
