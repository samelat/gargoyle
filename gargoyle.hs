-- import Network.Socket

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.IO
import Control.Exception 
import Control.Concurrent
import Control.Concurrent.Chan
-- import Control.Monad
-- import Control.Monad.Fix (fix)

import Data.Word
import Data.ByteString as D
 
type Msg = (Int, String)

-- Session handshake
data SessionRequest  = SessionRequest  Word8 Word8 [Word8] deriving (Show)
data SessionResponse = SessionResponse Word8 Word8         deriving (Show)

-- Command's request/response
data CommandRequest  = CommandRequest  Word8 Word8 Word8 Word8 [Word8] Word16 deriving (Show)
data CommandResponse = CommandResponse Word8 Word8 Word8 Word8 [Word8] Word16 deriving (Show)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1080 iNADDR_ANY)
    listen sock 2

    socksConnection sock 0

socksConnection :: Socket -> Int -> IO ()
socksConnection sock nr = do
    conn <- accept sock
    forkIO (runConn conn nr)
    socksConnection sock $! nr+1

-- Print information about the client's connection
informConnection :: (SockAddr) -> IO ()
informConnection (SockAddrInet port host_ip) = do
    (inet_ntoa host_ip) >>= System.IO.putStrLn

makeSessionRequest :: [Word8] -> SessionRequest
makeSessionRequest (version:nmethos:methods) = SessionRequest version nmethos methods

getSessionRequest :: Socket -> IO (SessionRequest)
getSessionRequest sock = do
    buffer <- recv sock 3
    return $ makeSessionRequest $ unpack buffer


printByteString :: [Word8] -> IO ()
printByteString (x:y:z:xs) = do
    System.IO.putStrLn $ show x
    System.IO.putStrLn $ show y
    System.IO.putStrLn $ show z

runConn :: (Socket, SockAddr) -> Int -> IO ()
runConn (sock, sock_addr) nr = do
    
    informConnection sock_addr

    response <- getSessionRequest sock

    System.IO.putStrLn $ show response

    sClose sock