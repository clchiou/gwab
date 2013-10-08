-- Copyright (C) 2013 Che-Liang Chiou.

module Main where

import Haste
import Haste.Prim

import Telnet

foreign import ccall js_logging :: JSString -> IO ()

foreign import ccall js_resolve :: JSString ->
                                   JSFun (JSString -> IO ()) ->
                                   IO ()

foreign import ccall js_connect :: JSString ->
                                   Int ->
                                   JSFun (Int -> Int -> IO ()) ->
                                   IO ()


logging :: String -> IO ()
logging = js_logging . toJSStr


resolve :: String -> (String -> IO ()) -> IO ()
resolve host cb = js_resolve (toJSString host) (mkCallback js_cb)
    where js_cb = cb . fromJSStr


connect :: String -> Int -> (Int -> Int -> IO ()) -> IO ()
connect addr port cb = js_connect (toJSStr addr) port (mkCallback cb)


main = do
    logging "Hello world"
    resolve "ptt.cc" (\addr -> connect addr 23 showConnect)
    where showConnect socketId resultCode =
            logging $ "socketId="   ++ (show socketId) ++ ", " ++
                      "resultCode=" ++ (show resultCode)
