-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    convertEncoding,

    clearInterval,
    setInterval,
) where


import Haste
import Haste.Prim


foreign import ccall js_convertEncoding :: JSString -> JSString ->
                                           JSFun (JSString -> IO ()) -> IO ()

foreign import ccall js_setInterval   :: Int -> JSFun (IO ()) -> IO Int
foreign import ccall js_clearInterval :: Int -> IO ()


convertEncoding :: String -> String -> (String -> IO ()) -> IO ()
convertEncoding encoding str cb =
    js_convertEncoding (toJSStr encoding) (toJSStr str)
                       (mkCallback $ cb . fromJSStr)


clearInterval :: Int -> IO ()
clearInterval = js_clearInterval


setInterval :: Int -> IO () -> IO Int
setInterval msecs cb = js_setInterval msecs (mkCallback cb)
