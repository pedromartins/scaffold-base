module Scaffold.Util where

import Network.Socket
import Network.DNSSD

import Scaffold.Types
import Scaffold.DNSSD
import Scaffold.Config

depreqToCaps :: DepReq -> [DepReq]
depreqToCaps (d `And` d') = (depreqToCaps d ++ depreqToCaps d')
depreqToCaps d = [d]

getRegistry :: IO String
getRegistry = do
  mr <- resolveOne "_scaffold._tcp"
  case mr of
    Nothing -> readRegistryConfig
    Just (SResolved fn ht p t) -> do
      h <- fmap (getAddrHost . addrAddress . head) $ getAddrInfo (Just defaultHints { addrFamily = AF_INET }) (Just ht) Nothing
      return $ "http://" ++ h ++ ":" ++ (show p) ++ "/"

getAddrHost = takeWhile (/=':') . show

