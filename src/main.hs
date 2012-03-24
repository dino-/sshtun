-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Directory
import System.Exit
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)
import System.Posix.Signals
import System.Posix.User

import Paths_sshtun
import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log
import Sshtun.Switch
import Sshtun.Tunnel


confPath :: FilePath
confPath = "/etc/sshtun.conf"


main :: IO ()
main = do
   checkEnv

   shared <- atomically $ newTVar (Stopped, Stop)

   mapM_ ( \signal -> installHandler signal
      (Catch $ handler shared) Nothing ) [sigINT, sigTERM]

   readFile confPath >>= parseConf >>= either exitFail
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


checkEnv :: IO ()
checkEnv = do
   -- root user check
   euid <- getEffectiveUserID
   when (euid /= 0) $
      exitFail "This service must be run by the root user"

   -- conf file check
   doesFileExist confPath >>= (flip unless $ do
      instDocPath <- getDataFileName "INSTALL"
      let msg = init . unlines $
            [ "Unable to find conf file at " ++ confPath
            , "This could mean sshtun isn't installed fully"
            , "Please see " ++ instDocPath ++ " to complete installation"
            ]
      exitFail msg
      )
