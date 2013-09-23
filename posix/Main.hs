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
        hSetBinaryMode,
        hSetBuffering,
        hSetEcho,
        stderr,
        stdin,
        stdout)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Telnet


main :: IO ()
main = do
    args <- getArgs
    let host = args !! 0
    let port = args !! 1

    let nvt0 = Telnet.defaultNvt
    hPutStrLn stderr (show nvt0)

    -- Setup stdin/stdout
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    if Telnet.getBinary nvt0
    then hSetBinaryMode stdin  True >>
         hSetBinaryMode stdout True
    else return ()

    runWithSocket host port $ \socket -> do
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= step nvt0 socket . Telnet.parse
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetChar stdin >>= send socket . Char8.singleton >>
    keyboardInput socket


step :: Telnet.Nvt -> Socket -> [Telnet.Packet] -> IO ()
step nvt socket (p:ps) = do
    let (nvt', mp) = Telnet.negotiate nvt p
    case p of
        Telnet.Text text -> hPutStr   stdout text
        Telnet.Iac  iac  -> hPutChar  stdout iac
        otherwise        -> hPutStrLn stderr ("< " ++ (show p))

    -- Debug output
    case mp of
        Just p' -> hPutStrLn stderr ("> " ++ (show p')) >> sendPacket socket p'
        Nothing -> return ()
    if nvt /= nvt' then hPutStrLn stderr (show nvt') else return ()

    -- Put new state into effect...
    if Telnet.getBinary nvt /= Telnet.getBinary nvt'
    then hSetBinaryMode stdin  (Telnet.getBinary nvt') >>
         hSetBinaryMode stdout (Telnet.getBinary nvt')
    else return ()
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


runWithSocket :: String -> String -> (Socket -> IO a) -> IO a
runWithSocket host port proc = withSocketsDo $ do
    addrInfos <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    proc sock
