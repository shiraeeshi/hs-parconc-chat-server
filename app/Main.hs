{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, forkFinally)
import Text.Printf (printf, hPrintf)
import Control.Monad (when, unless, forever, void, join)
import qualified Control.Exception as E
import Network.Socket
  ( socket
  , SocketType(Stream)
  , setSocketOption
  , SocketOption(ReuseAddr)
  , bind
  , SockAddr(SockAddrInet)
  , listen
  , accept
  , socketToHandle
  , HostName
  , ServiceName
  , addrFlags
  , addrSocketType
  , withSocketsDo
  , close
  , defaultHints
  , AddrInfoFlag(AI_PASSIVE)
  , getAddrInfo
  , withFdSocket
  , setCloseOnExecIfNeeded
  , addrAddress
  , addrFamily
  , addrProtocol )
import System.IO
  ( hSetBuffering
  , hGetLine
  , hPutStrLn
  , hClose
  , Handle
  , hSetNewlineMode
  , universalNewlineMode
  , BufferMode (LineBuffering)
  , IOMode(ReadWriteMode)
  )
import Control.Monad.STM (STM, atomically, retry, throwSTM)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar')
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
--import Network.Socket.ByteString (recv, sendAll)
--import qualified Data.ByteString as S

import qualified Data.Map as Map
import Data.Map (Map)

import Server (Server(..), newServer, broadcast)
import Client (Client(..), ClientName, Message(..), newClient, sendMessage)

-- from https://hackage.haskell.org/package/network-3.1.2.1/docs/Network-Socket.html
-- and  https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

port :: Int
port = 44444

main :: IO ()
main = do
  server <- newServer
  runTCPServer Nothing (show port) talk server
--   where
--     othertalk s = do
--       msg <- recv s 1024
--       unless (S.null msg) $ do
--         sendAll s msg
--         othertalk s

runTCPServer :: Maybe HostName -> ServiceName -> (Handle -> Server -> IO a) -> Server -> IO a
runTCPServer mhost port handler server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
      -- $ \(conn, _peer) -> void $
      $ \(conn, (SockAddrInet port host)) -> void $ do
        printf "Accepted connection from %s: %s\n" (show host) (show port)
        -- printf "Accepted connection from %s: %s\n" "test" "test"
        handle <- socketToHandle conn ReadWriteMode
        -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
        -- but 'E.bracketOnError' above will be necessary if some
        -- non-atomic setups (e.g. spawning a subprocess to handle
        -- @conn@) before proper cleanup of @conn@ is your case
        -- prev version:
        -- forkFinally (handler conn) (const $ gracefulClose conn 5000)
        -- forkFinally (handler handle) (const $ gracefulClose conn 5000)
        -- forkFinally (handler handle) (\_ -> hClose handle)
        forkFinally (handler handle server) (\_ -> hClose handle)

-- main :: IO ()
-- main = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   bind sock (SockAddrInet port iNADDR_ANY)
--   listen sock 2
--   printf "Listening on port %d\n" port
--   forever $ do
--     (socket, (SockAddrInet port host)) <- accept sock
--     printf "Accepted connection from %s: %s\n" host (show port)
--     handle <- socketToHandle socket ReadWriteMode
--     forkFinally (talk handle) (\_ -> hClose handle)

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle LineBuffering
    readName
  where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
        then readName
        else E.mask $ \restore -> do
                ok <- checkAddClient server name handle
                case ok of
                  Nothing -> restore $ do
                    hPrintf handle "The name %s is in use, please choose another\n" name
                    readName
                  Just client ->
                    restore (runClient server client)
                      `E.finally` removeClient server name

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server $ Notice (name ++ " has connected")
            return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
    if clientName == "admin"
      then race admin receive
      else race server receive
    return ()
  where
    receive = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage client (Command msg)

    admin = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason -> return $
          hPutStrLn clientHandle $ "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage serv client msg
            when continue admin

    server = join $ atomically $ do
      isPaused <- readTVar isPausedTV
      when (isPaused) retry
      k <- readTVar clientKicked
      case k of
        Just reason -> return $
          hPutStrLn clientHandle $ "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage serv client msg
            when continue server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
    Notice msg         -> output $ "*** " ++ msg
    Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
    Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
    Command msg ->
      case words msg of
        ["/kick", who] -> do
          atomically $ kick server who clientName
          return True
        "/tell" : who : what -> do
          tell server client who (unwords what)
          return True
        ["/pause"] -> do
          if clientName == "admin"
            then atomically $ pause server
            else hPutStrLn clientHandle "you need admin privileges to perform this operation"
          return True
        ["/resume"] -> do
          if clientName == "admin"
            then atomically $ resume server
            else hPutStrLn clientHandle "you need admin privileges to perform this operation"
          return True
        ["/pauseKicks"] -> do
          if clientName == "admin"
            then atomically $ pauseKicks server
            else hPutStrLn clientHandle "you need admin privileges to perform this operation"
          return True
        ["/resumeKicks"] -> do
          if clientName == "admin"
            then atomically $ resumeKicks server
            else hPutStrLn clientHandle "you need admin privileges to perform this operation"
          return True
        ["/quit"] ->
          return False
        ('/':_):_ -> do
          hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
          return True
        _ -> do
          atomically $ broadcast server $ Broadcast clientName msg
          return True
  where
    output s = do hPutStrLn clientHandle s; return True

-- Basic operations

pause :: Server -> STM ()
pause server@Server{..} = writeTVar isPausedTV True

resume :: Server -> STM ()
resume server@Server{..} = writeTVar isPausedTV False

pauseKicks :: Server -> STM ()
pauseKicks server@Server{..} = writeTVar isKickPausedTV True

resumeKicks :: Server -> STM ()
resumeKicks server@Server{..} = writeTVar isKickPausedTV False

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing -> return False
    Just client -> sendMessage client msg >> return True

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
    then return ()
    else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  kickPaused <- readTVar isKickPausedTV
  when (kickPaused) retry
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)
