-- Copyright: 2011 Dino Morelli
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


{- This is an ugly hack to delay things until after switchWatcher 
   reads the flag status
-}
tunnelStart :: ConfMap -> TVar Shared -> IO ()
tunnelStart c s = sleep 5 >> tunnelManager c s


{- This is the tunnel managing loop, tries to keep it running,
   if desired
-}
tunnelManager :: ConfMap -> TVar Shared -> IO ()
tunnelManager conf shared = do
   putStrLn "tunnelManager entered"

   state <- atomically . readTVar $ shared

   case state of
      (Stopped, Run) -> do
         -- Tunnel is stopped, but we'd like it to be started

         -- Get various values from conf for the tunnel
         sshPort <- confInt "sshPort" conf
         remotePort <- confInt "remotePort" conf
         localPort <- confInt "localPort" conf
         remoteUser <- confString "remoteUser" conf
         remoteHost <- confString "remoteHost" conf

         putStrLn "Starting tunnel now"
         ph <- runCommand $ printf "ssh -p %d -N -R %d:localhost:%d %s@%s"
            sshPort remotePort localPort remoteUser remoteHost
         atomically $ writeTVar shared (Running ph, Run)

         -- Then, we wait. Possibly for a long time
         _ <- waitForProcess ph

         -- Tunnel has (possibly unexpectedly) stopped, make a note of this
         putStrLn "tunnelManager unblocked"
         -- Read this again, may have changed during long wait
         (_, dst) <- atomically . readTVar $ shared
         atomically $ writeTVar shared (Stopped, dst)
      _ -> return ()

   confInt "tunnelRetryDelay" conf >>= sleep
   tunnelManager conf shared
