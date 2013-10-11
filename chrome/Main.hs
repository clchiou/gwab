-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste

import Control.Monad (when)

import Telnet

import Socket
import Store
import Utils


host :: String
host = "aardmud.org"


port :: Int
port = 4000


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


gwab :: IO ()
gwab = do
    message <- getString incomingMessage
    when (length message > 0)
         (mapM_ (writeLog . show) $ parse message)
    putString incomingMessage ""


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
