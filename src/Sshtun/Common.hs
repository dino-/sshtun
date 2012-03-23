-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Common
   where

import Control.Concurrent
import Control.Concurrent.STM
import System.Process


data TunnelState
   = Running ProcessHandle
   | Stopped

data DesiredState = Run | Stop

type Shared = (TunnelState, DesiredState)


sleep :: Int -> IO ()
sleep = threadDelay . (*) 1000000


run :: TVar Shared -> IO ()
run shared = do
   atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (tst, Run)


stop :: TVar Shared -> IO ()
stop shared = do
   tst <- atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (Stopped, Stop)
      return tst
   stopPh tst


stopPh :: TunnelState -> IO ()
stopPh (Running ph) = terminateProcess ph
stopPh Stopped      = return ()
