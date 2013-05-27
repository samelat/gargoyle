import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)

import Data.Int
 
type Msg = (Int, String)

-- +----+-----+-------+------+----------+----------+
-- |VER | CMD |  RSV  | ATYP | DST.ADDR | DST.PORT |
-- +----+-----+-------+------+----------+----------+
-- | 1  |  1  | X'00' |  1   | Variable |    2     |
-- +----+-----+-------+------+----------+----------+
data IP = IPv4 Int32 | IPv6 Int64 deriving (Show)
data Socks5ClientResponse = Socks5ClientResponse Int8 Int8 Int8 Int8 IP Int16 deriving (Show)
 
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
	(inet_ntoa host_ip) >>= putStrLn

getSocks5ClientResponse :: Socket -> IO ()
getSocks5ClientResponse sock = do
	putStrLn "getSocks5Response"
    -- Socks5ClientResponse 10


runConn :: (Socket, SockAddr) -> Int -> IO ()
runConn (sock, sock_addr) nr = do
    -- let broadcast msg = writeChan chan (nr, msg)
    informConnection sock_addr

    a <- recv sock 256

    putStrLn a

    sClose sock