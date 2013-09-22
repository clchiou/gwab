-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Concurrent (forkIO, killThread)
import Data.ByteString.Char8 as Char8 (
        null,
        pack,
        unpack)
import Network.Socket (
        Socket,
        SocketType(Stream),
        addrAddress,
        addrFamily,
        connect,
        defaultProtocol,
        getAddrInfo,
        sClose,
        socket,
        withSocketsDo)
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hGetLine, stdin)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Telnet


host = "aardmud.org"
port = 4000


main :: IO ()
main = runWithSocket $ \socket -> do
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= procPackets socket . Telnet.parse
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetLine stdin >>=
    sendAll socket . Char8.pack . (++ "\n") >>
    keyboardInput socket


procPackets :: Socket -> [Telnet.Packet] -> IO ()
procPackets socket (p:ps) = do
    case p of
        Telnet.Text text -> putStrLn text
        Telnet.Will opt  -> putStrLn ("WILL " ++ show opt) >>
                            sendAll socket (serialize (Telnet.Dont opt))
        Telnet.Do   opt  -> putStrLn ("DO   " ++ show opt) >>
                            sendAll socket (serialize (Telnet.Wont opt))
        otherwise        -> putStrLn (show p)
    procPackets socket ps
procPackets _ _ = putStrLn "End!" >> return ()


serialize = Char8.pack . Telnet.serialize


lazyRecvAll :: Socket -> IO String
lazyRecvAll socket = unsafeInterleaveIO $
    recv socket 1024 >>= \s ->
    if Char8.null s
    then sClose socket >> return ""
    else lazyRecvAll socket >>= return . ((unpack s) ++)


runWithSocket :: (Socket -> IO a) -> IO a
runWithSocket proc = withSocketsDo $ do
    addrInfos <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    proc sock
