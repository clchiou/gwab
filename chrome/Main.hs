-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste

import Control.Monad (when)
import Data.Char

import Telnet
import Terminal (Sequence(..))
import qualified Terminal (parse)

import JQuery
import Socket
import Store
import Utils


host :: String
host = "ptt.cc"


port :: Int
port = 23


timerId :: String
timerId = "timerId"


incomingMessage :: String
incomingMessage = "incomingMessage"


poll :: Int -> IO ()
poll fd = recv fd cb where
    cb resultCode message = do
        writeLog "poll: Polling..."
        if resultCode == 0
        then writeLog "poll: Could not read from socket"
        else when (length message > 0)
                  (getString incomingMessage >>=
                   putString incomingMessage . (++ message))


onKeypress :: Int -> Int -> IO ()
onKeypress fd key = send fd msg cb where
    msg  = if key' == '\n' then "\r\0" else [key']
    key' = chr key
    cb bytes = writeLog $ "onKeypress: msg=" ++ msg ++ " bytes=" ++ show bytes


gwab :: IO ()
gwab = do
    message <- getString incomingMessage
    when (length message > 0)
         (mapM_ processPacket $ parse' message)
    putString incomingMessage ""


processPacket :: Packet -> IO ()
processPacket (PacketText text) = mapM_ processText $ parse'' text
processPacket packet            = writeLog $ show packet


processText :: Sequence -> IO ()
processText (TextSequence text) = convertEncoding "big5" text
                                  (writeLog . ("text: " ++))
processText escape@_            = writeLog $ show escape


parse' :: String -> [Packet]
parse' str@(_:_) = packet : parse' rest
    where (packet, rest)               = unpack $ parse str
          unpack (Right result       ) = result
          unpack (Left  (Err reason) ) = error reason
          unpack (Left  NeedMoreInput) = error "Need more input"
parse' _ = []


parse'' :: String -> [Sequence]
parse'' str@(_:_) = sequence : parse'' rest
    where (sequence, rest)             = unpack $ Terminal.parse str
          unpack (Right result       ) = result
          unpack (Left  (Err reason) ) = error reason
          unpack (Left  NeedMoreInput) = error "Need more input"
parse'' _ = []


main = onStartup where

    onStartup =
        resolve host onResolveAddress

    onResolveAddress addr =
        connect addr port onConnect


onConnect fd resultCode = onConnect' where

    onConnect' = do
        writeLog $ "main: "      ++
                   "fd="         ++ show fd         ++ ", " ++
                   "resultCode=" ++ show resultCode
        when (fd /= 0) start

    start = do
        j "#target"
            >>= keypress (onKeypress fd)
            >>= keyup    (writeLog . ("keyup: "   ++) . show)
            >>= keydown  (writeLog . ("keydown: " ++) . show)

        putString incomingMessage ""

        timerId' <- setInterval 500 periodic
        putInt timerId timerId'

        setTimeout 10000 exit

    periodic = do
        poll fd
        gwab

    exit = do
        writeLog "main: Exit!"
        timerId' <- getInt timerId
        clearInterval timerId'
        disconnect fd
