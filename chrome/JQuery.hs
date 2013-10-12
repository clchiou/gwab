-- Copyright (C) 2013 Che-Liang Chiou.

module JQuery (
    j,
    keyup,
    keydown,
    keypress,
) where

import Haste
import Haste.Prim


foreign import ccall js_jquery :: JSString -> IO (JQuery)
foreign import ccall js_keyup :: JQuery -> JSFun (Int -> IO ()) -> IO ()
foreign import ccall js_keydown :: JQuery -> JSFun (Int -> IO ()) -> IO ()
foreign import ccall js_keypress :: JQuery -> JSFun (Int -> IO ()) -> IO ()


newtype JQuery = JQuery JSAny


j :: String -> IO JQuery
j selector = js_jquery (toJSStr selector)


keyup :: (Int -> IO()) -> JQuery -> IO ()
keyup f jq = js_keyup jq (mkCallback f)


keydown :: (Int -> IO()) -> JQuery -> IO ()
keydown f jq = js_keydown jq (mkCallback f)


keypress :: (Int -> IO()) -> JQuery -> IO ()
keypress f jq = js_keypress jq (mkCallback f)
