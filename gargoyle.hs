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
import Data.ByteString (unpack, pack)

type Msg = (Int, String)

-- Valid Authentication Methods
-- 0 - Without Auth
auth_methods = [0]

-- Session handshake
data SessionRequest  = SessionRequest {
    sreq_version :: Int,
    sreq_count   :: Int,
    sreq_methods :: [Int] } deriving (Show)

data SessionResponse = SessionResponse {
    sres_version :: Int,
    sres_method  :: Int }   deriving (Show)

-- Command's request/response
data CommandRequest  = CommandRequest {
    creq_version   :: Int,
    creq_command   :: Int,
    creq_addr_type :: Int,
    creq_dst_addr  :: Int,
    creq_dst_port  :: Int } deriving (Show)

type CommandResponse = CommandRequest

-- Print information about the client's connection
informConnection :: (SockAddr) -> IO ()
informConnection (SockAddrInet port host_ip) = do
    (inet_ntoa host_ip) >>= System.IO.putStrLn

-- #################### TCP Connecton Command #################### --

-- #################### Session Management #################### --
makeSessionRequest :: [Int] -> SessionRequest
makeSessionRequest (version:nmethos:methods) = SessionRequest version nmethos methods

getSessionRequest :: Socket -> IO (SessionRequest)
getSessionRequest sock = do
    buffer <- recv sock 3
    return $ makeSessionRequest $ map (\x -> fromIntegral x :: Int) $ unpack buffer

isValidSessionRequest :: SessionRequest -> Bool
isValidSessionRequest (SessionRequest version count methods)
    | version /= 4 && version /= 5 = False
    | count == 0 = False
    | length methods == 0 = False
    | length methods /= (fromIntegral count :: Int) = False
    | otherwise = True

-- We must handle the IOError exception
sendSessionResponse :: Socket -> SessionResponse -> IO (Int)
sendSessionResponse sock session_response = do
    sendAll sock $ pack [(fromIntegral (sres_version session_response) :: Word8),
                         (fromIntegral (sres_method  session_response) :: Word8)]
    return 0

replySessionRequest :: Socket -> SessionRequest -> IO (Bool)
replySessionRequest sock request_session
    | not $ isValidSessionRequest request_session = reject
    | 0 `elem` (sreq_methods request_session) = withoutAuth
    | otherwise = reject
    where
        withoutAuth = do
            error <- sendSessionResponse sock $ SessionResponse (sreq_version request_session) 0
            putStrLn $ show error
            if error == 0 then
                return True
            else
                return False
        reject = do
            sendSessionResponse sock $ SessionResponse (sreq_version request_session) 0xff
            return False

waitForSession

serveConnection :: (Socket, SockAddr) -> Int -> IO ()
serveConnection (sock, sock_addr) nr = do
    
    informConnection sock_addr

    session_request <- getSessionRequest sock
    System.IO.putStrLn $ show session_request

    -- MIRAR SI NO HAY NA MANERA MAS "ELEGANTE" DE HACER ESTO
    result_value <- replySessionRequest sock session_request
    if result_value then
        putStrLn "Atendemos la solicitud"
    else
        putStrLn "Cerramos la conexion"

    sClose sock

socksConnection :: Socket -> Int -> IO ()
socksConnection sock nr = do
    conn <- accept sock
    forkIO (serveConnection conn nr)
    socksConnection sock $ nr + 1

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1080 iNADDR_ANY)
    listen sock 2

    socksConnection sock 0
