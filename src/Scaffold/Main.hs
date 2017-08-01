-- Copyright 2017 Pedro M. N. Martins, Julie A. McCann, Imperial College London 
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice, this
-- list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its contributors
-- may be used to endorse or promote products derived from this software without
-- specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Control.Concurrent
import Control.Monad

import System.Posix.Daemonize
import qualified System.Posix.Daemonize as SPD
import System.Process
import System.IO
import System.Environment

import Scaffold.Types
import Scaffold.Register
import Scaffold.Util

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
        r <- getRegistry
        let remoteRegister = remote r "register" :: NodeCapRecord -> IO ()
        (sensors, actuators) <- readConfig
        mapM (\(s,sd,u) -> remoteRegister (NodeCapRecord (Just ip) (Provides s) sd u)) sensors
        mapM (\(c,sc,u) -> remoteRegister (NodeCapRecord (Just ip) (IsCapableOf c) sc u)) actuators
        remoteRegister (NodeCapRecord (Just ip) Any "" "scaffold")
        threadDelay 60000000
    , name = Just "scaffold"
    , SPD.user = Just "scaffold"
    , group = Just "users"
    }

