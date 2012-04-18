-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Switch
   ( switchWatcher
   )
   where

import Control.Concurrent.STM
import Network.Curl
import Text.Printf

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log


switchWatcher :: Conf -> TVar Shared -> IO ()
switchWatcher conf shared = do
   response <- curlGetString (switchUrl conf) []
   switch shared =<< respToState response

   logM INFO "Switch watcher starting to wait"
   sleep $ switchPollInterval conf
   logM INFO "Switch watcher done waiting"
   switchWatcher conf shared


respToState :: (CurlCode, String) -> IO DesiredState
respToState (CurlOK, "1\n") = return Run
respToState (CurlOK, "0\n") = return Stop
respToState (code,   _    ) = do
   logM ERROR $ printf "Problem reading switch file: %s" (show code)
   return Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   logM INFO "Switch watcher setting desired state to Run"
   run shared

switch shared Stop = do
   logM INFO "Switch watcher setting desired state to Stop"
   stop shared
