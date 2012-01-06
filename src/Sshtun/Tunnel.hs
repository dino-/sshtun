-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Tunnel
   ( tunnelStart
   )
   where

import Control.Concurrent.STM
import System.Process
import Text.Printf

import Sshtun.Common
import Sshtun.Conf
import Sshtun.Log


{- This is an ugly hack to delay things until after switchWatcher 
   reads the flag status
-}
tunnelStart :: Conf -> TVar Shared -> IO ()
tunnelStart c s = sleep 5 >> tunnelManager c s


{- This is the tunnel managing loop, tries to keep it running,
   if desired
-}
tunnelManager :: Conf -> TVar Shared -> IO ()
tunnelManager conf shared = do
   logM DEBUG "tunnelManager entered"

   state <- atomically . readTVar $ shared

   case state of
      (Stopped, Run) -> do
         -- Tunnel is stopped, but we'd like it to be started

         logM INFO "Starting tunnel now"
         ph <- runCommand $ printf "ssh -p %d -N -R %d:localhost:%d %s %s@%s"
            (sshPort conf) (remotePort conf) (localPort conf)
            (addlSshArgs conf) (remoteUser conf) (remoteHost conf)
         atomically $ writeTVar shared (Running ph, Run)

         -- Then, we wait. Possibly for a long time
         _ <- waitForProcess ph

         -- Tunnel has (possibly unexpectedly) stopped, make a note of this
         logM INFO "tunnelManager unblocked"
         -- Read this again, may have changed during long wait
         (_, dst) <- atomically . readTVar $ shared
         atomically $ writeTVar shared (Stopped, dst)
      _ -> return ()

   sleep $ tunnelRetryDelay conf
   tunnelManager conf shared
