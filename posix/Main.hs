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

    -- The default value of binaryMode is not RFC's default value, but we
    -- fould it useful in practice.
    let nvt0 = Telnet.Nvt {
        Telnet.binaryMode = Just (True,  doBinaryMode),
        Telnet.echo       = Just (False, doEcho),
        Telnet.supGoAhead = Nothing
    }
    hPutStrLn stderr (show nvt0)

    Telnet.applyOption $ Telnet.binaryMode nvt0
    Telnet.applyOption $ Telnet.echo       nvt0
    Telnet.applyOption $ Telnet.supGoAhead nvt0

    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering

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
    let (nvt', mp) = Telnet.step nvt p
    case p of
        Telnet.PacketText text -> hPutStr   stdout text
        otherwise              -> hPutStrLn stderr ("< " ++ (show p))

    -- Debug output
    case mp of
        Just p' -> hPutStrLn stderr ("> " ++ (show p')) >> sendPacket socket p'
        Nothing -> return ()
    if nvt /= nvt' then hPutStrLn stderr (show nvt') else return ()

    -- Put new state into effect...
    Telnet.doEdgeTrigger Telnet.binaryMode nvt nvt'
    Telnet.doEdgeTrigger Telnet.echo       nvt nvt'
    Telnet.doEdgeTrigger Telnet.supGoAhead nvt nvt'

    step nvt' socket ps
step _ _ _ = return ()


doBinaryMode :: Bool -> IO ()
doBinaryMode mode =
    hSetBinaryMode stdin  mode >>
    hSetBinaryMode stdout mode


doEcho :: Bool -> IO ()
doEcho = hSetEcho stdout . not


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
