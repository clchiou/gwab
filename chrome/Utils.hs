-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    logging,
) where

import Haste
import Haste.Prim


foreign import ccall js_logging :: JSString -> IO ()


logging :: String -> IO ()
logging = js_logging . toJSStr
