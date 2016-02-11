
import Control.Concurrent
import Control.Monad

import System.Posix.Daemonize
import qualified System.Posix.Daemonize as SPD
import System.Process
import System.IO
import System.Environment

import Scaffold.Types
import Scaffold.Register

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

main = do
  ip <- getIp
  args <- getArgs
  let serviced' =
        case args of
          []       -> serviced
          isTest:_ -> if isTest == "-t" then ($()) . program else serviced
  serviced' $ simpleDaemon
    { program = \_ -> forever $ do
        -- TODO: get ip (dynip) of registry
        (_, oh, _, _) <- runInteractiveCommand "curl http://www.doc.ic.ac.uk/~pm1108/scaffold/dynIP"
        r <- hGetContents oh
        let remoteRegister = remote r "register" :: NodeCapRecord -> IO ()
        (sensors, actuators) <- readConfig
        mapM (\(s,sd,u) -> remoteRegister (NodeCapRecord (Just ip) (Provides s) sd u)) sensors
        mapM (\(c,sc,u) -> remoteRegister (NodeCapRecord (Just ip) (IsCapableOf c) sc u)) actuators
        threadDelay 60000000
    , name = Just "scaffold"
    , SPD.user = Just "scaffold"
    , group = Just "users"
    }

