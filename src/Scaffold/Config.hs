{-# LANGUAGE ScopedTypeVariables #-}
module Scaffold.Config where

import Control.Exception
import System.Directory

import qualified Data.Configurator as Cfg
import Data.Configurator.Types
import qualified Data.Text as T

getScaffoldConf :: IO Config
getScaffoldConf = do
  hdir <- handle (\(e :: SomeException) -> return "/") getHomeDirectory
  Cfg.load [ Cfg.Optional "/etc/scaffold.conf", Cfg.Optional $ hdir ++ "/.scaffold.conf" ]

readScannerConfig :: IO String
readScannerConfig = do
   cfg <- getScaffoldConf
   dbpath <- Cfg.lookupDefault "." cfg (T.pack "dbpath")
   return $ dbpath ++ "/scanner.db"

readPortConfig :: IO Integer
readPortConfig = do
   cfg <- getScaffoldConf
   fmap read $ Cfg.lookupDefault "1234" cfg (T.pack "port")

readRegistryConfig :: IO String
readRegistryConfig = do
  cfg <- getScaffoldConf
  Cfg.lookupDefault "http://127.0.0.1:1234/" cfg (T.pack "registry")
