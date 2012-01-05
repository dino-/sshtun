-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log
import Sshtun.Switch
import Sshtun.Tunnel


main :: IO ()
main = readFile "/etc/sshtun.conf" >>=
   parseConf >>= either exitFail start


exitFail :: String -> IO ()
exitFail msg = do
   putStrLn msg
   exitWith $ ExitFailure 1


start :: Conf -> IO ()
start conf = do
   initLogging (logFile conf) (logPriority conf)

   logM NOTICE "sshtun starting"

   shared <- atomically $ newTVar (Stopped, Stop)
   _ <- forkIO $ tunnelStart conf shared
   switchWatcher conf shared

   logM NOTICE "sshtun stopping"
