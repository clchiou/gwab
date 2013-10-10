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


poller :: Int -> IO ()
poller fd = recv fd cb where
    cb resultCode message = do
        writeLog "poller: Poll..."
        if resultCode == 0
        then writeLog "poller: Could not read from socket"
        else when (length message > 0)
                  (writeLog $ "poller: message=" ++ message)


main = onStartup where

    onStartup =
        resolve host onResolveAddress

    onResolveAddress addr =
        connect addr port onConnect

    onConnect fd resultCode = do
        writeLog $ "main: "      ++
                   "fd="         ++ show fd         ++ ", " ++
                   "resultCode=" ++ show resultCode
        when (fd /= 0) (start fd)

    start fd = do
        timerId' <- setInterval 500 (poller fd)
        putInt timerId timerId'
        setTimeout 10000 (exit fd)

    exit fd = do
        writeLog "main: Exit!"
        timerId' <- getInt timerId
        clearInterval timerId'
        disconnect fd
