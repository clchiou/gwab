-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste

import Control.Monad (when)

import Telnet

import Socket
import Utils


host :: String
host = "aardmud.org"


port :: Int
port = 4000


main = resolve host onResolvedAddress
    where
        onResolvedAddress addr =
            connect addr port onConnected
        onConnected fd resultCode = do
            writeLog $ "onConnected: " ++
                       "fd="           ++ show fd         ++ ", " ++
                       "resultCode="   ++ show resultCode
            setTimeout 10000 (onTimeout fd)
            when (fd /= 0) $ watch fd (onWatch fd 0)
        onWatch fd cnt = do
            message <- recv fd
            writeLog $ "onWatch: message=" ++ message
            if cnt < 2
            then watch fd (onWatch fd (cnt + 1))
            else disconnect fd
        onTimeout fd = do
            writeLog $ "onTimeout: fd=" ++ show fd
            disconnect fd
