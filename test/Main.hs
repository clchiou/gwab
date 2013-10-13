module Main where

import qualified TestTelnet (main)
import qualified TestTerminal (main)


main = do
    putStrLn "Test Telnet module..."
    TestTelnet.main
    putStrLn "Test Terminal module..."
    TestTerminal.main
