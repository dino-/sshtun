-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Sshtun.Log
   ( initLogging, logM, logTest
   , Priority (..)  -- Re-exported from System.Log
   )
   where

import System.Log.Formatter
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( fileHandler )
import qualified System.Log.Logger as L ( logM )
import           System.Log.Logger hiding ( logM )


{- Convenience wrapper for logging
-}
logM :: Priority -> String -> IO ()
logM = L.logM rootLoggerName


{- Set up logging
-}
initLogging :: FilePath -> Priority -> IO ()
initLogging logFile logPriority = do
   h <- fileHandler logFile logPriority >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
   updateGlobalLogger rootLoggerName $ setHandlers [h]
   updateGlobalLogger rootLoggerName $ setLevel logPriority


{- Test function to generate every kind of log message
-}
logTest :: IO ()
logTest = do
   logM DEBUG     "log test message 1 of 8"
   logM INFO      "log test message 2 of 8"
   logM NOTICE    "log test message 3 of 8"
   logM WARNING   "log test message 4 of 8"
   logM ERROR     "log test message 5 of 8"
   logM CRITICAL  "log test message 6 of 8"
   logM ALERT     "log test message 7 of 8"
   logM EMERGENCY "log test message 8 of 8"
