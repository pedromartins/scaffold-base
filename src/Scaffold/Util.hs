module Scaffold.Util where

import Network.Socket

import Scaffold.Types
import Scaffold.Config

depreqToCaps :: DepReq -> [DepReq]
depreqToCaps (d `And` d') = (depreqToCaps d ++ depreqToCaps d')
depreqToCaps d = [d]

getRegistry :: IO String
getRegistry = readRegistryConfig
