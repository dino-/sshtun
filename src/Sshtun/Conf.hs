-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-} 

{- |
   Simple module for loading config files
-}
module Sshtun.Conf
   ( Conf (..)
   , parseConf
   )
   where

import Control.Monad.Error
import Data.Map hiding ( map )
import Data.Maybe ( catMaybes )
import Prelude hiding ( lookup )
import System.Log
import Text.Regex ( matchRegex, mkRegex )


type ConfMap = Map String String

data Conf = Conf
   { logFile :: String
   , logPriority :: Priority
   , switchUrl :: String
   , switchPollInterval :: Int
   , sshPort :: Int
   , localPort :: Int
   , remotePort :: Int
   , remoteUser :: String
   , remoteHost :: String
   , addlSshArgs :: String
   , tunnelRetryDelay :: Int
   }
   deriving Show

emptyConf :: Conf
emptyConf = Conf
   { logFile = ""
   , logPriority = DEBUG
   , switchUrl = ""
   , switchPollInterval = 0
   , sshPort = 0
   , localPort = 0
   , remotePort = 0
   , remoteUser = ""
   , remoteHost = ""
   , addlSshArgs = ""
   , tunnelRetryDelay = 0
   }


extractConfItem :: MonadError String m =>
   ConfMap -> (String, String -> m b) -> m b
extractConfItem cm (k, f) =
   maybe (throwError $ "Missing config field: " ++ k)
      f $ lookup k cm


parseConf :: Monad m => String -> m (Either String Conf)
parseConf entireConf = runErrorT $ do
   mutatorActions <- mapM (extractConfItem cm) exts
   foldM (\c f -> f c) emptyConf mutatorActions

   where
      cm = parseToMap entireConf

      exts =
         [ ( "logFile"
           , (\s -> return (\c -> return $ c { logFile = s }))
           )
         , ( "logPriority"
           , (\s -> return (\c -> do
               p <- strToPriority s
               return $ c { logPriority = p }
               ))
           )
         , ( "switchUrl"
           , (\s -> return (\c -> return $ c { switchUrl = s }))
           )
         , ( "switchPollInterval"
           , (\s -> return (\c -> do
               n <- readE ("switchPollInterval, unable to parse: " ++ s) s
               return $ c { switchPollInterval = n }
               ))
           )
         , ( "sshPort"
           , (\s -> return (\c -> do
               n <- readE ("sshPort, unable to parse: " ++ s) s
               return $ c { sshPort = n }
               ))
           )
         , ( "localPort"
           , (\s -> return (\c -> do
               n <- readE ("localPort, unable to parse: " ++ s) s
               return $ c { localPort = n }
               ))
           )
         , ( "remotePort"
           , (\s -> return (\c -> do
               n <- readE ("remotePort, unable to parse: " ++ s) s
               return $ c { remotePort = n }
               ))
           )
         , ( "remoteUser"
           , (\s -> return (\c -> return $ c { remoteUser = s }))
           )
         , ( "remoteHost"
           , (\s -> return (\c -> return $ c { remoteHost = s }))
           )
         , ( "addlSshArgs"
           , (\s -> return (\c -> return $ c { addlSshArgs = s }))
           )
         , ( "tunnelRetryDelay"
           , (\s -> return (\c -> do
               n <- readE ("tunnelRetryDelay, unable to parse: " ++ s) s
               return $ c { tunnelRetryDelay = n }
               ))
           )
         ]


readE :: (MonadError e m, Read a) => e -> String -> m a
readE msg s = case reads s of
   ((x, ""):[]) -> return x
   _            -> throwError msg


{- |
   Parse config file data into a simple (Map String String).

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >  --- file end ---

   becomes:

   >  fromList [("foo","one"),("bar",""),("baz-blorp","2")]

   Comments (prefixed with #) and blank lines in the config file 
   are discarded.
-}
parseToMap :: String -> ConfMap
parseToMap entireConf =
   fromList $ map listToPair
      $ catMaybes $ map (matchRegex re) $ lines entireConf

   where
      listToPair [k, v] = (k, v)
      listToPair _      = undefined  -- Should never happen

      re = mkRegex "^([^#][^=]*)=?(.*)"


strToPriority :: MonadError String m => String -> m Priority
strToPriority "DEBUG"     = return DEBUG
strToPriority "INFO"      = return INFO
strToPriority "NOTICE"    = return NOTICE
strToPriority "WARNING"   = return WARNING
strToPriority "ERROR"     = return ERROR
strToPriority "CRITICAL"  = return CRITICAL
strToPriority "ALERT"     = return ALERT
strToPriority "EMERGENCY" = return EMERGENCY
strToPriority x           = throwError $ "Invalid logPriority: " ++ x
