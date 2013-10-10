-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    clearInterval,
    setInterval,
) where


import Haste
import Haste.Prim


foreign import ccall js_setInterval :: Int -> JSFun (IO ()) -> IO Int
foreign import ccall js_clearInterval :: Int -> IO ()


setInterval :: Int -> IO () -> IO Int
setInterval msecs cb = js_setInterval msecs (mkCallback cb)


clearInterval :: Int -> IO ()
clearInterval = js_clearInterval
