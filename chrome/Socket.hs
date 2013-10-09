-- Copyright (C) 2013 Che-Liang Chiou.

module Socket (
    resolve,
    connect,
    disconnect,
    recv,
    send,
) where

import Haste
import Haste.Prim


foreign import ccall js_resolve :: JSString ->
                                   JSFun (JSString -> IO ()) ->
                                   IO ()


foreign import ccall js_connect :: JSString ->
                                   Int ->
                                   JSFun (Int -> Int -> IO ()) ->
                                   IO ()


foreign import ccall js_disconnect :: Int -> IO ()


foreign import ccall js_recv :: Int -> IO JSString


foreign import ccall js_send :: Int -> JSString -> JSFun (Int -> IO ()) -> IO ()


resolve :: String -> (String -> IO ()) -> IO ()
resolve host cb = js_resolve (toJSString host) (mkCallback js_cb)
    where js_cb = cb . fromJSStr


connect :: String -> Int -> (Int -> Int -> IO ()) -> IO ()
connect addr port cb = js_connect (toJSStr addr) port (mkCallback cb)


disconnect :: Int -> IO ()
disconnect = js_disconnect


recv :: Int -> IO String
recv fd = js_recv fd >>= return . fromJSStr


send :: Int -> String -> (Int -> IO ()) -> IO ()
send fd msg cb = js_send fd (toJSStr msg) (mkCallback cb)
