module Main where

import Control.Concurrent (forkIO, forkFinally)
import Text.Printf (printf)
import Control.Monad (unless, forever, void)
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
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, Handle, BufferMode (LineBuffering), IOMode(ReadWriteMode) )
--import Network.Socket.ByteString (recv, sendAll)
--import qualified Data.ByteString as S

-- from https://hackage.haskell.org/package/network-3.1.2.1/docs/Network-Socket.html
-- and  https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

port :: Int
port = 44444

main :: IO ()
main = runTCPServer Nothing (show port) talk
--   where
--     othertalk s = do
--       msg <- recv s 1024
--       unless (S.null msg) $ do
--         sendAll s msg
--         othertalk s

runTCPServer :: Maybe HostName -> ServiceName -> (Handle -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
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
        -- forkFinally (server conn) (const $ gracefulClose conn 5000)
        -- forkFinally (server handle) (const $ gracefulClose conn 5000)
        -- forkFinally (server handle) (\_ -> hClose handle)
        forkFinally (server handle) $ \_ -> do
          putStrLn "about to close handle"
          hClose handle
          close conn

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

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    loop
  where
    loop = do
      line <- hGetLine h
      if line == "end"
        then hPutStrLn h "Thank you for using the Haskell doubling service."
        else do hPutStrLn h $ show $ 2 * (read line :: Integer)
                loop
