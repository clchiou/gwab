module Main where

import Data.String.Utils
import Test.QuickCheck

import Telnet


-- This makes all IAC quoted...
quote :: String -> String
quote = replace "\255" "\255\255"


main :: IO ()
main = do
    quickCheck (\s -> (concat . map serialize . parse . quote) s == s)
