-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Concurrent (forkIO, killThread)
import Data.ByteString.Char8 as Char8 (
        ByteString,
        null,
        pack,
        singleton,
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
import Network.Socket.ByteString (
        recv,
        send,
        sendAll)
import System.IO (
        BufferMode(..),
        hGetChar,
        hPutChar,
        hPutStr,
        hPutStrLn,
        hSetBuffering,
        hSetEcho,
        stderr,
        stdin,
        stdout)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Telnet


host = "aardmud.org"
port = 4000


main :: IO ()
main = runWithSocket $ \socket -> do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= step Telnet.defaultNvt socket . Telnet.parse
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetChar stdin >>= send socket . Char8.singleton >>
    keyboardInput socket


step :: Telnet.Nvt -> Socket -> [Telnet.Packet] -> IO ()
step nvt socket (p:ps) = do
    case p of
        Telnet.Text text -> hPutStr   stdout text
        Telnet.Iac  iac  -> hPutChar  stdout iac
        otherwise        -> hPutStrLn stderr ("< " ++ (show p))
    let (nvt', mp) = Telnet.negotiate nvt p
    hPutStrLn stderr (show nvt')
    case mp of
        Just p' -> hPutStrLn stderr ("> " ++ (show p')) >> sendPacket socket p'
        Nothing -> return ()
    if Telnet.getEcho nvt /= Telnet.getEcho nvt'
    then hSetEcho stdout (not $ Telnet.getEcho nvt')
    else return ()
    step nvt' socket ps
step _ _ _ = return ()


sendPacket :: Socket -> Telnet.Packet -> IO ()
sendPacket socket packet = sendAll socket $ serialize packet


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
