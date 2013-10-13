module TestTerminal where

import Data.String.Utils
import Test.QuickCheck

import Terminal


main :: IO ()
main = do
    quickCheck prop_2char_sequence
    quickCheck prop_text_sequence


parse' :: String -> [Sequence]

parse' str@(_:_) = sequence : parse' rest where
    (sequence, rest)             = unpack $ parse str
    unpack (Right result       ) = result
    unpack (Left  (Err reason) ) = error reason
    unpack (Left  NeedMoreInput) = error "Need more input"

parse' [] = []


prop_2char_sequence :: Bool
prop_2char_sequence = all test ['\64'..'\95'] where
    test c = parse' ['\27', c] == [EscapeSequence [c]]


prop_text_sequence :: String -> Bool
prop_text_sequence str =
    if null str'
    then parse' str' == []
    else parse' str' == [TextSequence str']
    where str'     = unescape str
          unescape = replace "\27" ""
