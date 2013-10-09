-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste
import Haste.Prim

import Control.Monad (when)

import Telnet

import Socket
import Utils


main = do
    resolve "aardmud.org" (\addr ->
            connect addr 4000 (\fd resultCode -> do
                logging $ "fd="         ++ show fd         ++ ", " ++
                          "resultCode=" ++ show resultCode ++ "\n"
                when (fd /= 0) (doSomething fd "")))


-- XXX: This does NOT work because it does not yield and so socket never gets
-- read!
doSomething fd message =
    recv fd >>= \msg ->
    let message' = message ++ msg
    in do
    logging $ "message: " ++ message' ++ "\n"
    if length message' < 32
    then doSomething fd message'
    else disconnect fd
