-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Simple module for loading config files
-}
module Sshtun.Conf
   ( ConfMap, parseToMap
   , confString
   , confInt
   , confPri
   )
   where

import Data.Map hiding ( map )
import Data.Maybe ( catMaybes )
import Prelude hiding ( lookup )
import System.Log
import Text.Regex ( matchRegex, mkRegex )


type ConfMap = Map String String


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


confString :: String -> ConfMap -> IO String
confString k m = maybe (error $ "Missing config field: " ++ k)
   return $ lookup k m


confInt :: String -> ConfMap -> IO Int
confInt k m = fmap read $ confString k m


confPri :: String -> ConfMap -> IO Priority
confPri k m = confString k m >>= strToPriority


strToPriority :: String -> IO Priority
strToPriority "DEBUG"     = return DEBUG
strToPriority "INFO"      = return INFO
strToPriority "NOTICE"    = return NOTICE
strToPriority "WARNING"   = return WARNING
strToPriority "ERROR"     = return ERROR
strToPriority "CRITICAL"  = return CRITICAL
strToPriority "ALERT"     = return ALERT
strToPriority "EMERGENCY" = return EMERGENCY
strToPriority x           = error $ "Invalid logPriority: " ++ x
