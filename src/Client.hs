{-# LANGUAGE RecordWildCards #-}
module Client
  ( ClientName
  , Client(..)
  , Message(..)
  , newClient
  , sendMessage
  ) where

import System.IO (Handle)
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan)

type ClientName = String

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

data Client = Client
  { clientName :: ClientName
  , clientHandle :: Handle
  , clientKicked :: TVar (Maybe String)
  , clientSendChan :: TChan Message }

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k}

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
