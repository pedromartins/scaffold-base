
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import System.Posix.Daemonize
import System.Process
import System.IO
import System.Environment

import Network.MessagePack.Client

import qualified Data.ByteString.Char8 as B

import Scaffold.Types
import Scaffold.Register
import Scaffold.Util

main = do
  ip <- getIp
  args <- getArgs
  let daemonize' =
        case args of
          []       -> daemonize
          isTest:_ -> if isTest == "-t" then id else daemonize
  daemonize' . forever $ do
    -- TODO: get ip (dynip) of registry
    (_, oh, _, _) <- runInteractiveCommand "curl http://www.doc.ic.ac.uk/~pm1108/scaffold/dynIP"
    r <- hGetContents oh
    execClient (B.pack r) 5000 $ do
      let remoteRegister = call "register" :: NodeCapRecord -> Client Int
      (sensors, actuators) <- liftIO readConfig
      mapM (\(s,sd,u) -> remoteRegister (NodeCapRecord (Just ip) (Provides s) sd u)) sensors
      mapM (\(c,sc,u) -> remoteRegister (NodeCapRecord (Just ip) (IsCapableOf c) sc u)) actuators
      liftIO $ threadDelay 6000000

