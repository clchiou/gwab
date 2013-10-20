module Main where

import qualified TestScreen (main)
import qualified TestTelnet (main)
import qualified TestTerminal (main)


main = do
    putStrLn "Test Screen module..."
    TestScreen.main
    putStrLn "Test Telnet module..."
    TestTelnet.main
    putStrLn "Test Terminal module..."
    TestTerminal.main
