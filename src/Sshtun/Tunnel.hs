-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Tunnel
   ( tunnelManager
   )
   where

import Control.Concurrent.STM
import System.Process

import Sshtun.Common


tunnelManager :: TVar Shared -> IO ()
tunnelManager shared = do
   state <- atomically . readTVar $ shared

   case state of
      (Stopped, Run) -> do
         -- Tunnel is stopped, but we'd like it to be started
         putStrLn "Starting tunnel now"
         ph <- runCommand "ssh -p 22 -N -R 2022:localhost:22 dino@ui3.info"
         atomically $ writeTVar shared (Running ph, Run)

         -- Then, we wait. Possibly for a long time
         _ <- waitForProcess ph

         -- Tunnel has (possibly unexpectedly) stopped, make a note of this
         putStrLn "tunnelManager unblocked"
         -- Read this again, may have changed during long wait
         (_, dst) <- atomically . readTVar $ shared
         atomically $ writeTVar shared (Stopped, dst)
      _ -> return ()

   sleep 10
   tunnelManager shared
