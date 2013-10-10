-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste

import Control.Monad (when)

import Telnet

import Socket


host :: String
host = "aardmud.org"


port :: Int
port = 4000


poller :: Int -> Int -> IO ()
poller interval fd = poller' 2 where

    poller' cnt = recv fd (cb cnt)

    cb cnt resultCode message =
        if resultCode == 0
        then writeLog "poller: Could not read from socket"
        else when (length message > 0)
                  (writeLog $ "poller: message=" ++ message)
             >>
             when (cnt > 0)
                  (nextPoll (cnt - 1))

    nextPoll cnt = setTimeout interval (poller' cnt)


main = onStartup where

    onStartup =
        resolve host onResolveAddress

    onResolveAddress addr =
        connect addr port onConnect

    onConnect fd resultCode = do
        writeLog $ "onConnect: " ++
                   "fd="         ++ show fd         ++ ", " ++
                   "resultCode=" ++ show resultCode
        when (fd /= 0) (start fd)

    start fd = do
        poller 500 fd
        setTimeout 10000 (disconnect fd)
