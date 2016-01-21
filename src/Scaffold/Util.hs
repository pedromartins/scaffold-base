module Scaffold.Util where

import Scaffold.Types

depreqToCaps :: DepReq -> [DepReq]
depreqToCaps (d `And` d') = (depreqToCaps d ++ depreqToCaps d')
depreqToCaps d = [d]
