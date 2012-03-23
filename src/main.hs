-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import System.Exit
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)
import System.Posix.Signals

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log
import Sshtun.Switch
import Sshtun.Tunnel


main :: IO ()
main = do
   shared <- atomically $ newTVar (Stopped, Stop)

   mapM_ ( \signal -> installHandler signal
      (Catch $ handler shared) Nothing ) [sigINT, sigTERM]

   readFile "/etc/sshtun.conf" >>= parseConf >>= either exitFail
      (\c -> startDaemon (localDaemonUser c) $ sshtunMain c shared)


exitFail :: String -> IO ()
exitFail msg = do
   putStrLn msg
   exitWith $ ExitFailure 1


sshtunMain :: Conf -> TVar Shared -> () -> IO ()
sshtunMain conf shared _ = do
   initLogging (logFile conf) (logPriority conf)

   logM NOTICE "sshtun starting"

   _ <- forkIO $ tunnelStart conf shared
   switchWatcher conf shared


startDaemon :: String -> (() -> IO ()) -> IO ()
startDaemon localUser p = serviced $ simpleDaemon
   { program = p
   , user = Just localUser
   }


handler :: TVar Shared -> IO ()
handler shared = do
   logM NOTICE "sshtun stopping"
   stop shared
