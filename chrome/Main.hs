-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste

import Telnet

foreign import ccall js_logging :: JSString -> IO ()


logging :: String -> IO ()
logging = js_logging . toJSString


main = logging "Hello world"
