
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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,
  GADTs, UndecidableInstances, OverlappingInstances #-}
module Scaffold.Prelude where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import Scaffold.Drivers.POSIX
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Monad
import System.IO
import Scaffold.Types

modify :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
modify k v [] = [(k,v)]
modify k v (kv@(k',_):kvs')
  | k Prelude.== k' = (k,v) : kvs'
  | otherwise = kv : modify k v kvs'

broker = fromMaybe "localhost"

mosquittoTopic = "test"
topic s = "/" Prelude.++ s
loadSpec = undefined

-- TODO: Reliable pub.
pub :: Maybe String -> String -> String -> IO ()
pub mb t m = do
 threadDelay 1000000
 putStrLn $ "mosquitto_pub  -h " Prelude.++ broker mb Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t Prelude.++ " -m '" Prelude.++ m Prelude.++ "'"
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -h " Prelude.++ broker mb Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t Prelude.++ " -m '" Prelude.++ m Prelude.++ "'"
 threadDelay 1000000
 waitForProcess ph
 Prelude.return ()

-- TODO: Timeout sub with default action
sub :: Maybe String -> String -> IO String
sub mb t = do
  -- FIXME: The following code assumes one reading per line
  putStrLn $ "mosquitto_sub -h " Prelude.++ broker mb Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t
  (_, oh, _, ph) <- runInteractiveCommand $ "mosquitto_sub -h " Prelude.++ broker mb Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t
  l <- hGetLine $ oh
  terminateProcess ph
  Prelude.return l

executeCommand :: [(DepReq, Driver)] -> Command -> IO ()
executeCommand drivers c = do
  (_, _, _, ph) <- runInteractiveCommand (Data.Maybe.fromJust $ Prelude.lookup (IsCapableOf c) drivers)
  waitForProcess ph
  Prelude.return ()

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
(>>=) = (Prelude.>>=)

(>>) :: (Monad m) => m a -> m b -> m b
(>>) = (Prelude.>>)

(==) :: (Eq a) => a -> a -> Bool
(==) = (Prelude.==)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (Prelude..)

(++) :: [a] -> [a] -> [a]
(++) = (Prelude.++)

return :: (Monad m) => a -> m a
return = Prelude.return

unit :: ()
unit = ()

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup = Prelude.lookup

readIORef = Data.IORef.readIORef

ret :: a -> IO a
ret = Prelude.return

threshold :: String -> Bool
threshold = ((>500) Prelude.. Prelude.read)

execve = undefined

show :: (Show a) => a -> String
show = Prelude.show

concat :: [[a]] -> [a]
concat = Prelude.concat

fromJust :: Maybe a -> a
fromJust = Data.Maybe.fromJust

read :: (Read a) => String -> a
read = Prelude.read

bind :: (Monad m) => m a -> (a -> m b) -> m b
bind = (Prelude.>>=)

class Ap a b c | a b -> c where
  ap :: a -> b -> c

instance (a ~ a', b ~ b') => Ap (IO (a -> b)) (IO a') (IO b') where
  ap mf mx = mf <*> mx

instance (a ~ a', b ~ b') => Ap (IO (a -> IO b)) (IO a') (IO b') where
  ap mk ma = join (mk <*> ma)

-- Currently broken, special case added to parser for 'forever'
-- instance (a ~ a', b ~ b') => Ap (IO (IO a -> IO b)) (IO a') (IO b') where
--   ap mk ma = join (mk <*> (Prelude.return ma))

type IntT = Integer
