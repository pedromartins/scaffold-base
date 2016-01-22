{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
module Scaffold.Types where

import Data.Data
import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType

import Data.ByteString
import Data.Data

type DataQuery = String
type Command = String
type Ident = String
type Requirement = String
type Node = String

data DepReq = Provides DataQuery
            | IsCapableOf Command
            | Fulfills Requirement
            | And DepReq DepReq
            | Any
            deriving (Data, Typeable, Eq, Show, Read)

data Message = DataMessage DataQuery
             | CommandMessage Command
             | HeapMessage String Integer [Ident]
             | ResultMessage String Integer Program
             deriving (Eq, Show)

data Program = PVar Ident
             | PLam Ident Program
             | PApp Program Program
             | PIf Program Program Program
             | PWhile Program Program
             | Pub Message
             | Sub Message
             | Read DataQuery
             | PWith Requirement Program
             | PConstr Ident
             | Seq Program Program
             | PExecve String
             deriving (Eq, Show)

-- TODO:Should be a newtype: ByteString is not specific enough
type Option = String

type Frontend = [Option] -> ByteString -> IO [(Program, DepReq)]
type Backend = [Option] -> [DepReq] -> (Program, DepReq) -> IO ByteString

-- TODO:Should be a newtype: String is not specific enough
type Driver = String

type NodeId = Int
type NodeAddress = String
data NodeCapRecord = NodeCapRecord { host :: Maybe NodeAddress, cap :: DepReq, driver :: String, user :: String }
  deriving (Show, Data, Typeable)

instance XmlRpcType DepReq where
  toValue = ValueString . show
  fromValue (ValueString s) = return $ read s
  getType _ = TUnknown

instance XmlRpcType () where
  toValue () = ValueInt 0
  fromValue _ = return ()
  getType _ = TUnknown

-- Note that THDeriveXmlRpcType handles Maybes in its own special way
-- (existence of field)
$(asXmlRpcStruct ''NodeCapRecord)

instance (Show a, Read a) => (XmlRpcType (Maybe a)) where
  toValue = ValueString . show
  fromValue (ValueString s) = return $ read s
  getType _ = TUnknown

