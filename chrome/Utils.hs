-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    timeout,
) where

import Haste


foreign import ccall js_timeout :: Int -> JSFun (IO ()) -> IO ()


timeout t cb = js_timeout t (mkCallback cb)
