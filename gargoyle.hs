-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
-- MA 02110-1301, USA.

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

import System.Console.CmdArgs

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

data SockData = SockData {
    sdata_host_ip   :: Word32,
    sdata_host_port :: Word16 } deriving (Show)

-- Command's request/response
data CommandRequest  = CommandRequest {
    creq_version   :: Int,
    creq_command   :: Int,
    creq_sock_data :: SockData } deriving (Show)

type CommandResponse = CommandRequest

-- Print information about the client's connection
informConnection :: (SockAddr) -> IO ()
informConnection socket_data = do
    -- ip <- inet_ntoa host_ip
    putStrLn "\n------------------- New connecton -------------------"
    -- putStrLn $ "[!] Client IP: " ++ ip ++ ":" ++ (show port)
    putStrLn $ "[!] Client IP: " ++ (show socket_data)

-- #################### TCP Connecton Command Handler #################### --

-- Get the information about the host to which to connect
-- In case of domain name (addr_type 3) we resolve it and use the first IP address
getConnectionInfo :: [Word8] -> IO (SockData)
getConnectionInfo buffer
    | addr_type == 1 = return $ SockData host $ port 8
    | addr_type == 3 = sock_addr_from_hostname
    where sock_addr_from_hostname = do
            host_ip <- hostname_to_ip $ between 5 hostname_size
            return $ SockData host_ip (port (hostname_size + 5))
          port index = foldr (\x acc -> (shiftL acc 8) + (fromIntegral x :: Word16)) 0 (between (hostname_size + 5) 2)
          host = foldl (\acc x -> (shiftL acc 8) + (fromIntegral x :: Word32)) 0 $ between 4 4
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


getCommandRequest :: Socket -> IO (CommandRequest)
getCommandRequest sock = do
    buffer <- recv sock 1024
    request <- makeCommandRequest $ unpack buffer
    return request

sendCommandResponse :: Socket -> CommandResponse -> IO (Int)
sendCommandResponse sock command_response = do
    sendAll sock $ pack $ [(fromIntegral (creq_version command_response) :: Word8), -- Version
                           (0 :: Word8),                                            -- Succeeded
                           (0 :: Word8),                                            -- Reserved
                           (1 :: Word8)] ++
                           [0x7f, 0x00, 0x00, 0x01] ++
                           [0x04, 0x38]
    return 0

replayCommandRequest :: Socket -> CommandRequest -> IO (Bool)
replayCommandRequest sock command_session = do

    sendCommandResponse sock command_session

    return True

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
            if error == 0 then
                return True
            else
                return False
        reject = do
            sendSessionResponse sock $ SessionResponse (sreq_version request_session) 0xff
            return False


-- ################## Stream forwarding ################# --

forwardStreams :: Socket -> SockData -> IO ()
forwardStreams sock remote_host = do

    let sock_data = SockAddrInet (PortNum (sdata_host_port remote_host)) (sdata_host_ip remote_host)

    putStrLn $ "[!] Connecting to remote host: " ++ (show sock_data)

    tmp_sock <- socket AF_INET Stream 0
    connect tmp_sock sock_data

    -- sendAll tmp_sock $ Char8.pack tmp

    -- buffer <- recv tmp_sock 4096

    -- putStrLn $ show buffer

    forkIO $ streamForwarding sock tmp_sock

    streamForwarding tmp_sock sock

    where
        streamForwarding in_sock out_sock = do
            buffer <- recv in_sock 4096
            let size = length $ unpack buffer
            -- putStrLn $ "[!] Buffer size is: " ++ (show $ size)
            if (size > 0) then do
                putStrLn $ "[!] Sending bytes: " ++ (show $ size)
                sendAll out_sock buffer
                streamForwarding in_sock out_sock
            else do
                sClose in_sock


-- ################## General Handling ################# --
serveConnection :: (Socket, SockAddr) -> Int -> IO ()
serveConnection (sock, sock_addr) nr = do
    
    informConnection sock_addr

    session_request <- getSessionRequest sock
    -- putStrLn $ show session_request

    result_value <- replySessionRequest sock session_request

    if result_value then do

        command_request <- getCommandRequest sock
        -- putStrLn $ show command_request

        replayCommandRequest sock command_request

        forwardStreams sock (creq_sock_data command_request)

    else do
        sClose sock
        putStrLn "[!] Connection closed"

socksConnection :: Socket -> Int -> IO ()
socksConnection sock nr = do
    conn <- accept sock
    forkIO (serveConnection conn nr)
    socksConnection sock $ nr + 1

main_loop :: IO ()
main_loop = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1080 iNADDR_ANY)
    listen sock 2

    socksConnection sock 0

data Args = Args {
                wordlist :: String,
                url :: String,
                outfile :: String,
                disable_bruteforce :: Bool
            }
            deriving (Show, Data, Typeable)


arguments = Args{wordlist = "wordlist" &= typFile &= help "The wordlist to use",
                url = def &= argPos 0 &= typ "URL",
                outfile = "dirhound.out" &= typFile &= help "Output file",
                disable_bruteforce = False &= help "Don't bruteforce, only crawl"
                } &= program "DirHound" &= summary "DirHound web server directory bruteforcer"

main = do
        parsed <- cmdArgs arguments
        let wordlist_name = wordlist parsed
            uri = url parsed
            in do wlist <- if disable_bruteforce parsed then return [] else readWordlist wordlist_name
                  hfile <- openFile (outfile parsed) WriteMode
                  case parseURI uri of 
                    Just x -> processLoop (makeCrawler x wlist) hfile
                    Nothing -> error "Not a valid URL"
                  hClose hfile