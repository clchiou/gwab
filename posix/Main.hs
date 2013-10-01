-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Applicative
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

import Platform (replace)

import Telnet
import Telnet.Utils


-- The default value of binary transmission is not RFC's default value, but we
-- fould it useful in practice.
nvt0 :: Nvt
nvt0  = NvtContext {
    binary     = NvtOptBool True,
    echo       = NvtOptBool False,
    windowSize = NvtOptPair (80, 24),
    termType   = NvtOptString "VT100",
    supGoAhead = NvtOptNothing
}


nvtOptDoer :: NvtContext (NvtOpt -> IO ())
nvtOptDoer  = NvtContext {
    binary     = \opt -> hSetBinaryMode stdin  (nvtOptBool opt) >>
                         hSetBinaryMode stdout (nvtOptBool opt),
    echo       = hSetEcho stdout . not . nvtOptBool,
    windowSize = hPutStrLn stderr . ("windowSize: " ++) . show . nvtOptPair,
    termType   = hPutStrLn stderr . ("termType: " ++) . nvtOptString,
    supGoAhead = undefined
}


doNvtOpt :: Nvt -> IO ()
doNvtOpt nvt = doNvt $ liftA2 (maybe' (return())) nvtOptDoer nvt
    where maybe' zero doer opt =
            if opt /= NvtOptNothing then doer opt else zero


main :: IO ()
main = do
    args <- getArgs
    let host = args !! 0
    let port = args !! 1

    hPutStrLn stderr (show nvt0)
    doNvtOpt nvt0

    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering

    runWithSocket host port $ \socket -> do
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= forever nvt0 socket . parse
    killThread threadId


keyboardInput :: Socket -> IO ()
keyboardInput socket =
    hGetChar stdin >>= send socket . Char8.pack . crlf >>
    keyboardInput socket


crlf :: Char -> String
crlf c = if c == '\n' then "\r\0" else [c]


forever :: Nvt -> Socket -> [Packet] -> IO ()
forever nvt socket (p:ps) = do
    -- Compute next state and output
    let (nvt', ps') = step nvt p
    let triggered   = liftA2 edge nvt nvt'
    case p of
        PacketText text -> hPutStr stdout text
        otherwise       -> return ()
    mapM_ (sendPacket socket) ps'
    doNvtOpt triggered

    -- Debug logs
    case p of
        PacketText text -> return ()
        otherwise       -> hPutStrLn stderr ("recv: " ++ show p)
    mapM_ (hPutStrLn stderr . ("send: " ++) . show) ps'
    if_ (nvt /= nvt') $
        (hPutStrLn stderr $ ("nvt : " ++) $ show nvt)
    if_ (triggered /= pure NvtOptNothing) $
        (hPutStrLn stderr $ ("trig: " ++) $ show triggered)

    -- Next iteration
    forever nvt' socket ps
forever _ _ _ = return ()


if_ :: Bool -> IO () -> IO ()
if_ pred io = if pred then io else return ()


sendPacket :: Socket -> Packet -> IO ()
sendPacket socket packet = sendAll socket $ serialize' packet


serialize' :: Packet -> ByteString
serialize' = Char8.pack . serialize


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
