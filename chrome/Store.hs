-- Copyright (C) 2013 Che-Liang Chiou.

-- Haste.Concurrent is really painful to use; let's do it my way.

module Store (
    getBool,
    getInt,
    getString,
    putBool,
    putInt,
    putString,
) where

import Haste
import Haste.Prim


foreign import ccall js_getBool   :: JSString -> IO Bool
foreign import ccall js_getInt    :: JSString -> IO Int
foreign import ccall js_getString :: JSString -> IO JSString
foreign import ccall js_putBool   :: JSString -> Bool     -> IO ()
foreign import ccall js_putInt    :: JSString -> Int      -> IO ()
foreign import ccall js_putString :: JSString -> JSString -> IO ()


getBool :: String -> IO Bool
getBool key = js_getBool (toJSStr key)


getInt :: String -> IO Int
getInt key = js_getInt (toJSStr key)


getString :: String -> IO String
getString key = js_getString (toJSStr key) >>= return . fromJSStr


putBool :: String -> Bool -> IO ()
putBool key val = js_putBool (toJSStr key) val


putInt :: String -> Int -> IO ()
putInt key val = js_putInt (toJSStr key) val


putString :: String -> String -> IO ()
putString key val = js_putString (toJSStr key) (toJSStr val)
