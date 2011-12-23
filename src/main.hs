-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log
import Sshtun.Switch
import Sshtun.Tunnel


main :: IO ()
main = do
   hSetBuffering stdout NoBuffering

   conf <- fmap parseToMap $ readFile "/etc/sshtun.conf"

   logFile <- confString "logFile" conf
   logPriority <- confPri "logPriority" conf
   initLogging logFile logPriority

   logM NOTICE "sshtun starting"
   logTest

   shared <- atomically $ newTVar (Stopped, Stop)
   _ <- forkIO $ tunnelStart conf shared
   switchWatcher conf shared

   logM NOTICE "sshtun stopping"
