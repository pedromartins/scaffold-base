{-# LANGUAGE CPP #-}
module Scaffold.Register where

import Control.Applicative
import Data.Maybe
import Control.Monad
import System.Directory
import System.IO
import System.Process
import Network.Socket
import qualified Data.Configurator as Cfg
import qualified Data.Text as T

import Scaffold.Types

type Attribute = (String, String)

-- STUB
queryCpu :: IO Attribute
queryCpu = return ("cpu", "N/A")

queryHD :: IO Attribute
queryHD = return ("HD", "N/A")

readSensorConfig :: FilePath -> IO (String, String, String)
readSensorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "sensor.query")
  driver <- Cfg.lookup cfg (T.pack "sensor.driver")
  user <- Cfg.lookup cfg (T.pack "sensor.user")
  return (fromJust query, fromJust driver, fromJust user)

readActuatorConfig :: FilePath -> IO (String, String, String)
readActuatorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "actuator.command")
  driver <- Cfg.lookup cfg (T.pack "actuator.driver")
  user <- Cfg.lookup cfg (T.pack "actuator.user")
  return (fromJust query, fromJust driver, fromJust user)

getFilesRec d = do
  nodes <- listDirectory d
  fmap concat . forM nodes $ \n -> do
    idn <- isDirectory n
    if idn then getFilesRec n else return [n]

-- TODO: Change so that the files can be anywhere within .scaffold.d
readConfig = do
  let rootDirectory = "/etc"
      sensorRoot = rootDirectory ++ "/scaffold.d/sensors"
      actuatorRoot = rootDirectory ++ "/scaffold.d/actuators"
  sensorRootExists <- isDirectory sensorRoot
  actuatorRootExists <- isDirectory actuatorRoot
  sensorPaths <- if sensorRootExists then getDirectoryContents sensorRoot else return []
  actuatorPaths <- if actuatorRootExists then getDirectoryContents actuatorRoot else return []
  sensorFiles <- filterM isFile . map (sensorRoot ++) $ sensorPaths
  actuatorFiles <- filterM isFile . map (actuatorRoot ++) $ actuatorPaths
  sensors <- mapM readSensorConfig sensorFiles
  actuators <- mapM readActuatorConfig actuatorFiles
  return (sensors, actuators)

-- Copied from directory-1.2.5.0 to avoid dependency hell
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    (filter f) <$> (getDirectoryContents path)
      where f filename = filename /= "." && filename /= ".."

isFile = doesFileExist
isDirectory = fmap not . doesFileExist

-- More NAT friendly. Should be replaced with proper socket code.
getIp = do
#ifdef darwin_HOST_OS
  (_, oh, _, _) <- runInteractiveCommand "ifconfig `route get www.doc.ic.ac.uk | grep interface | cut -d ':' -f 2` | grep 'inet ' | cut -d ' ' -f 2"
#else
  (_, oh, _, _) <- runInteractiveCommand "ip route get 8.8.8.8 | head -n 1 | awk '{print $NF}'"
#endif
  fmap init $ hGetContents oh

