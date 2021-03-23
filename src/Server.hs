{-# LANGUAGE RecordWildCards #-}
module Server (Server(..), newServer, broadcast) where

import Client (ClientName, Client, Message, sendMessage)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (mapM_)
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)

data Server = Server { clients :: TVar (Map ClientName Client)}

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server { clients = c}

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
