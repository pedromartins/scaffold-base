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
             | PLet [(Ident, Program)] Program
             | POp Ident
             | PIntLit Integer
             | PStringLit String
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

