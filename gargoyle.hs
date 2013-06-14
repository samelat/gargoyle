-- import Network.Socket

import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.IO
import Control.Exception 
import Control.Concurrent
import Control.Concurrent.Chan
-- import Control.Monad
-- import Control.Monad.Fix (fix)

import Data.Bits
import Data.Word
import Data.ByteString (pack, unpack)
import qualified Data.ByteString.Char8 as Char8

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
    creq_addr_type :: SockAddr } deriving (Show)

type CommandResponse = CommandRequest

-- Print information about the client's connection
informConnection :: (SockAddr) -> IO ()
informConnection (SockAddrInet port host_ip) = do
    (inet_ntoa host_ip) >>= System.IO.putStrLn

-- #################### TCP Connecton Command #################### --

getConnectionInfo :: [Word8] -> IO (SockAddr)
getConnectionInfo buffer
    | addr_type == 1 = return $ SockAddrInet (PortNum $ port 8) host
    | addr_type == 3 = sock_addr_from_hostname
    where sock_addr_from_hostname = do
            host_ip <- hostname_to_ip $ between 5 hostname_size
            return $ SockAddrInet (PortNum $ port (hostname_size + 5)) host_ip
          -- port index = fromIntegral (foldl (\acc x -> (shiftL acc 8) + x) 0 $ between index 2) :: Word16
		  port index = fromIntegral (foldl (\acc x -> (shiftL acc 8) + x) 0 $ between index 2) :: Word16
          host = fromIntegral (foldl (\acc x -> (shiftL acc 8) + x) 0 $ between 4 4) :: Word32
          addr_type  = fromIntegral (buffer !! 3) :: Int
          hostname_to_ip bytes = fmap hostAddress (getHostByName $ Char8.unpack $ pack bytes)
          between index count = fst $ splitAt count $ snd (splitAt index buffer)
          hostname_size = fromIntegral (buffer !! 4) :: Int

makeCommandRequest :: [Word8] -> IO (CommandRequest)
makeCommandRequest buffer = do
    connection_info <- getConnectionInfo buffer
    return $ CommandRequest version command connection_info
    where version   = fromIntegral (buffer !! 0) :: Int
          command   = fromIntegral (buffer !! 1) :: Int

getCommandRequest :: Socket -> IO (Int)
getCommandRequest sock = do
    -- Asumimos, por ahora, que siempre hablamos de IPv4
    buffer <- recv sock 1024
    request <- makeCommandRequest $ unpack buffer
    putStrLn $ show $ request
    --toWord32 tuple = foldl (\acc x -> (shiftL acc 8) + x) 0 tuple
    return 0

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

-- ################## General Handling ################# --
serveConnection :: (Socket, SockAddr) -> Int -> IO ()
serveConnection (sock, sock_addr) nr = do
    
    informConnection sock_addr

    session_request <- getSessionRequest sock
    System.IO.putStrLn $ show session_request

    -- MIRAR SI NO HAY UNA MANERA MAS "ELEGANTE" DE HACER ESTO
    result_value <- replySessionRequest sock session_request
    result_value2 <- getCommandRequest sock
    if result_value then
        putStrLn "Atendemos la solicitud"
    else do
        sClose sock
        putStrLn "Connection closed"



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
