-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Common
   where

import Control.Concurrent
import System.Process


data TunnelState
   = Running ProcessHandle
   | Stopped

data DesiredState
   = Run | Stop
   deriving Show

type Shared = (TunnelState, DesiredState)


sleep :: Int -> IO ()
sleep = threadDelay . (*) 1000000
