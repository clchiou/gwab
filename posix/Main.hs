-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Concurrent (forkIO, killThread)
import Data.ByteString.Char8 as Char8 (
        ByteString,
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
import System.IO (
        BufferMode(..),
        hGetLine,
        hPutChar,
        hPutStr,
        hPutStrLn,
        hSetBuffering,
        stderr,
        stdin,
        stdout)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Telnet


host = "aardmud.org"
port = 4000


main :: IO ()
main = runWithSocket $ \socket -> do
    hSetBuffering stdout NoBuffering
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= step Telnet.Nvt socket . Telnet.parse
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetLine stdin >>=
    sendAll socket . Char8.pack . (++ "\n") >>
    keyboardInput socket


step :: Telnet.Nvt -> Socket -> [Telnet.Packet] -> IO ()
step nvt socket (p:ps) = do
    let (nvt', mp) = Telnet.negotiate nvt p
    case mp of
        Just p' -> send socket p'
        Nothing -> return ()
    case p of
        Telnet.Text text -> hPutStr   stdout text
        Telnet.Iac  iac  -> hPutChar  stdout iac
        otherwise        -> hPutStrLn stderr (show p)
    step nvt' socket ps
step _ _ _ = return ()


send :: Socket -> Telnet.Packet -> IO ()
send socket packet = sendAll socket $ serialize packet


serialize :: Telnet.Packet -> ByteString
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
