{-# LANGUAGE CPP #-}
module Scaffold.DNSSD where

import Control.Monad
import Control.Concurrent
import Network.DNSSD
import System.IO
import System.Process

register :: String -> String -> String -> Integer -> String -> IO ProcessHandle
register name ty domain port txt = do
#ifdef darwin_HOST_OS
  spawnCommand $ "dns-sd -R \"" ++ name ++ "\" " ++ ty ++ " " ++ domain ++ " " ++ show port ++ " " ++ txt
#else
  spawnCommand $ "avahi-publish \"" ++ name ++ "\" " ++ ty ++ " " ++ show port ++ " " ++ txt
#endif

resolveOne :: String -> IO (Maybe SResolved)
resolveOne s = do
  things <- dnsBrowse s
  if null things
    then return Nothing
    else dnsResolve $ head things

