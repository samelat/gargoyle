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
import Data.ByteString (unpack)

type Msg = (Int, String)

-- Valid Authentication Methods
-- 0 - Without Auth
auth_methods = [0]

-- Session handshake
data SessionRequest  = SessionRequest  Int Int [Int] deriving (Show)
data SessionResponse = SessionResponse Word8 Word8         deriving (Show)

-- Command's request/response
data CommandRequest  = CommandRequest  Word8 Word8 Word8 Word8 [Word8] Word16 deriving (Show)
data CommandResponse = CommandResponse Word8 Word8 Word8 Word8 [Word8] Word16 deriving (Show)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1080 iNADDR_ANY)
    listen sock 2

    socksConnection sock 0

socksConnection :: Socket -> Int -> IO ()
socksConnection sock nr = do
    conn <- accept sock
    forkIO (runConn conn nr)
    socksConnection sock $ nr + 1

-- Print information about the client's connection
informConnection :: (SockAddr) -> IO ()
informConnection (SockAddrInet port host_ip) = do
    (inet_ntoa host_ip) >>= System.IO.putStrLn

makeSessionRequest :: [Int] -> SessionRequest
makeSessionRequest (version:nmethos:methods) = SessionRequest version nmethos methods

getSessionRequest :: Socket -> IO (SessionRequest)
getSessionRequest sock = do
    buffer <- recv sock 3
    return $ makeSessionRequest $ map (\x -> fromIntegral x :: Int) $ unpack buffer

printByteString :: [Word8] -> IO ()
printByteString (x:y:z:xs) = do
    System.IO.putStrLn $ show x
    System.IO.putStrLn $ show y
    System.IO.putStrLn $ show z

isValidSessionRequest :: SessionRequest -> Bool
isValidSessionRequest (SessionRequest version count methods)
    | version /= 4 && version /= 5 = False
    | count == 0 = False
    | length methods == 0 = False
    | length methods /= (fromIntegral count :: Int) = False
    | otherwise = True

replySessionRequest :: Socket -> SessionRequest -> IO (Bool)
replySessionRequest sock request_session
    | not $ isValidSessionRequest request_session = return False
    | 0 elem (methods request_session) = withoutAuth
    | otherwise = reject
    where
        methods (SessionRequest version count meths) = meths
        withoutAuth = do
            putStrLn $ show $ isValidSessionRequest request_session
            return True
        reject = do
            putStrLn $ show $ isValidSessionRequest request_session
            return True

runConn :: (Socket, SockAddr) -> Int -> IO ()
runConn (sock, sock_addr) nr = do
    
    informConnection sock_addr

    session_request <- getSessionRequest sock
    System.IO.putStrLn $ show session_request

    replySessionRequest sock session_request

    sClose sock