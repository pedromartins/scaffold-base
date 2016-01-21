
import System.Posix.Daemonize

main = daemonize . forever $ do
  -- TODO: get ip (dynip) of registry
  let r = "localhost"
  (sensors, actuators) <- readConfig
  mapM (\(s,sd) -> remote r "register" (NodeCapRecord (Just r) (Provides s) sd u)) sensors
  mapM (\(c,sc) -> remote r "register" (NodeCapRecord (Just r) (IsCapableOf c) sc u)) actuators
  threadDelay 60000



