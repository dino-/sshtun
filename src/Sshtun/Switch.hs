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

   logM INFO "switchWatcher starting to wait now"
   sleep $ switchPollInterval conf
   logM INFO "switchWatcher done waiting"
   switchWatcher conf shared


bodyToState :: (CurlCode, String) -> DesiredState
bodyToState (CurlOK, "1\n") = Run
bodyToState _               = Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   logM INFO "switch setting Run now"
   run shared

switch shared Stop = do
   logM INFO "switch setting Stop now"
   stop shared
