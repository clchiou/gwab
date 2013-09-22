-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Concurrent (forkIO, killThread)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8
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
import Network.Socket.ByteString (sendAll, recv)
import System.IO (hGetLine, stdin)
import System.IO.Unsafe (unsafeInterleaveIO)

import Telnet


host = "aardmud.org"
port = 4000


main :: IO ()
main = runWithSocket $ \socket -> do
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= procPackets socket . telnetRecv
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetLine stdin >>=
    sendAll socket . Char8.pack . (++ "\n") >>
    keyboardInput socket


procPackets :: Socket -> [TelnetPacket] -> IO ()
procPackets socket (p:ps) = do
    case p of
        TelnetText text -> putStrLn text
        TelnetWill opt  -> putStrLn ("WILL " ++ show opt) >>
                           sendAll socket (toByteString $ TelnetDont opt)
        TelnetDo   opt  -> putStrLn ("DO   " ++ show opt) >>
                           sendAll socket (toByteString $ TelnetWont opt)
        otherwise       -> putStrLn (show p)
    procPackets socket ps
procPackets _ _ = putStrLn "End!" >> return ()


lazyRecvAll :: Socket -> IO [ByteString]
lazyRecvAll socket = unsafeInterleaveIO $
    recv socket 1024 >>= \bs ->
    if Char8.null bs
    then sClose socket >> return []
    else do lazyRest <- lazyRecvAll socket
            return (bs:lazyRest)


runWithSocket :: (Socket -> IO a) -> IO a
runWithSocket proc = withSocketsDo $ do
    addrInfos <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    proc sock
