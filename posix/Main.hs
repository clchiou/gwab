-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Control.Applicative
import Control.Concurrent (
        forkIO,
        killThread)
import Control.Monad (
        foldM_,
        when)
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
        IOMode(..),
        hGetChar,
        hPutChar,
        hPutStr,
        hPutStrLn,
        hSetBinaryMode,
        hSetBuffering,
        hSetEcho,
        openFile,
        stdin,
        stdout)
import System.Environment (getArgs)
import System.IO.Unsafe (
        unsafeInterleaveIO,
        unsafePerformIO)

import Platform (replace)

import Telnet
import Telnet.Utils
import Terminal (Sequence(..))
import qualified Terminal (parse)


logFile = unsafePerformIO $ openFile "log" WriteMode

writeLog :: String -> IO ()
writeLog = hPutStrLn logFile


-- The default value of binary transmission is not RFC's default value, but we
-- fould it useful in practice.
nvt0 :: Nvt
nvt0 = fromList
    [(rfc856_BINARY_TRANSMISSION, NvtOptBool True),
     (rfc857_ECHO,                NvtOptBool False),
     (rfc858_SUPPRESS_GOAHEAD,    NvtOptAlways True),
     (rfc1073_WINDOW_SIZE,        NvtOptPair (80, 24)),
     (rfc1091_TERMINAL_TYPE,      NvtOptString "VT100")]


doNvtOpt :: NvtContext (NvtOpt -> IO ())
doNvtOpt = fromList
    [(rfc856_BINARY_TRANSMISSION,
      \opt -> hSetBinaryMode stdin  (nvtOptBool opt) >>
              hSetBinaryMode stdout (nvtOptBool opt)),
     (rfc857_ECHO,
      hSetEcho stdout . not . nvtOptBool),
     (rfc858_SUPPRESS_GOAHEAD,
      writeLog . ("supGoAhead: " ++) . show . nvtOptAlways),
     (rfc1073_WINDOW_SIZE,
      writeLog . ("windowSize: " ++) . show . nvtOptPair),
     (rfc1091_TERMINAL_TYPE,
      writeLog . ("termType: "   ++) . nvtOptString)]


doNvt :: Nvt -> IO ()
doNvt nvt = sequenceNvt (applyNvtOpt <$> doNvtOpt <*> nvt) where
    applyNvtOpt f NvtOptNothing = return ()
    applyNvtOpt f nvtOpt        = f nvtOpt


main :: IO ()
main = do
    args <- getArgs
    let host = args !! 0
    let port = args !! 1

    hSetBuffering logFile NoBuffering

    writeLog $ "nvt0: " ++ show nvt0
    doNvt nvt0

    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering

    runWithSocket host port $ \socket -> do
    threadId <- forkIO $ keyboardInput socket
    lazyRecvAll socket >>= foldM_ (callStep socket) nvt0 . parse'
    killThread threadId


callStep :: Socket -> (Nvt -> Packet -> IO Nvt)
callStep socket = step' where
    step' nvt packet = do
        -- Compute next state with step function
        let (nvt', ps) = step nvt packet

        -- Perform IO associated with state changes
        let triggered  = liftA2 edge nvt nvt'
        when (isText packet) $ hPutStr stdout (text packet)
        mapM_ (sendPacket socket) ps
        doNvt triggered

        -- Debug logs
        case packet of
            PacketText text -> mapM_ writeSequence $ parseSequence text
            otherwise       -> writeLog $ "recv: " ++ show packet
        mapM_ (writeLog . ("send: " ++) . show) ps
        when (nvt /= nvt')
             (writeLog $ "nvt : " ++ show nvt)
        when (triggered /= pure NvtOptNothing)
             (writeLog $ "trig: " ++ show triggered)

        -- Return new state
        return nvt'

    isText (PacketText _) = True
    isText _              = False

    writeSequence (TextSequence     text)    =
        writeLog $ "text: " ++ show text
    writeSequence (ControlCode command) =
        writeLog $ "cc  : " ++ show command
    writeSequence escape                     =
        writeLog $ "esc : " ++ show escape


parse' :: String -> [Packet]
parse' str@(_:_) = packet : parse' rest
    where (packet, rest)               = unpack $ parse str
          unpack (Right result       ) = result
          unpack (Left  (Err reason) ) = error reason
          unpack (Left  NeedMoreInput) = error "Need more input"
parse' _ = []


parseSequence :: String -> [Sequence]
parseSequence str@(_:_) = sequence : parseSequence rest
    where (sequence, rest)      = unpack $ Terminal.parse str
          unpack (Right result) = result
          unpack _              = error "Could not parse text"
parseSequence _ = []


keyboardInput :: Socket -> IO ()
keyboardInput socket = do
    c <- hGetChar stdin
    writeLog $ "key : " ++ show c
    send socket $ Char8.pack $ crlf c
    keyboardInput socket


crlf :: Char -> String
crlf c = if c == '\n' then "\r\0" else [c]


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
