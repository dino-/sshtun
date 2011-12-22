-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

import Sshtun.Common
import Sshtun.Switch
import Sshtun.Tunnel


main :: IO ()
main = do
   hSetBuffering stdout NoBuffering

   shared <- atomically $ newTVar initState
   _ <- forkIO $ tunnelManager shared
   switchWatcher shared


initState :: Shared
initState = (Stopped, Stop)
