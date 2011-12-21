-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Concurrent
import Control.Concurrent.STM
import Network.Curl
import System.IO
import System.Process

import Sshtun.Common
import Sshtun.Tunnel


main :: IO ()
main = do
   hSetBuffering stdout NoBuffering

   shared <- atomically $ newTVar initState
   _ <- forkIO $ tunnelManager shared
   switchWatcher shared


initState :: Shared
initState = (Stopped, Stop)


switchWatcher :: TVar Shared -> IO ()
switchWatcher shared = do
   swState <- curlGetString "http://ui3.info/d/tflag" []
   switch shared $ evalSwitch swState

   putStrLn "switchWatcher starting to wait now"
   sleep 20
   putStrLn "switchWatcher done waiting"
   switchWatcher shared


evalSwitch :: (CurlCode, String) -> DesiredState
evalSwitch (CurlOK, "1\n") = Run
evalSwitch _               = Stop


switch :: TVar Shared -> DesiredState -> IO ()

switch shared Run = do
   putStrLn "switch setting Run now"
   atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (tst, Run)

switch shared Stop = do
   putStrLn "switch setting Stop now"
   tst <- atomically $ do
      (tst, _) <- readTVar shared
      writeTVar shared (Stopped, Stop)
      return tst
   stop tst
            


stop :: TunnelState -> IO ()
stop (Running ph) = terminateProcess ph
stop Stopped      = return ()
