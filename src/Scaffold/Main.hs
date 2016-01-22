
import Control.Concurrent
import Control.Monad

import System.Posix.Daemonize
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
  let daemonize' =
        case args of
          []       -> daemonize
          isTest:_ -> if isTest == "-t" then id else daemonize
  daemonize' . forever $ do
    -- TODO: get ip (dynip) of registry
    (_, oh, _, _) <- runInteractiveCommand "curl http://www.doc.ic.ac.uk/~pm1108/scaffold/dynIP"
    r <- hGetContents oh
    let remoteRegister = remote ("http://" ++ r ++ ":80/~pedromartins/scnd.cgi") "register" :: NodeCapRecord -> IO ()
    (sensors, actuators) <- readConfig
    mapM (\(s,sd,u) -> remoteRegister (NodeCapRecord (Just ip) (Provides s) sd u)) sensors
    mapM (\(c,sc,u) -> remoteRegister (NodeCapRecord (Just ip) (IsCapableOf c) sc u)) actuators
    threadDelay 6000000

