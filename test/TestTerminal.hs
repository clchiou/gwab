module TestTerminal where

import Control.Applicative
import Control.Monad
import Data.String.Utils
import Test.QuickCheck

import Terminal


main :: IO ()
main = do
    quickCheck prop_2char_sequence
    quickCheck prop_text_sequence
    quickCheck prop_identity


parse' :: String -> [Sequence]

parse' str@(_:_) = sequence : parse' rest where
    (sequence, rest)             = unpack $ parse str
    unpack (Right result       ) = result
    unpack (Left  (Err reason) ) = error reason
    unpack (Left  NeedMoreInput) = error "Need more input"

parse' [] = []


serialize' :: [Sequence] -> String
serialize' = concat . map serialize


identity :: String -> String
identity = serialize' . parse'


prop_2char_sequence :: Bool
prop_2char_sequence = all test (['\64'..'\90'] ++ ['\92'..'\95']) where
    test c = parse' ['\27', c] == [EscapeSequence2C ['\27', c]]


prop_text_sequence :: String -> Bool
prop_text_sequence str =
    if null str'
    then parse' str' == []
    else parse' str' == [TextSequence str']
    where str'     = unescape str
          unescape = replace "\27" ""


prop_identity :: [Sequence] -> Bool
prop_identity ss = identity str == str where
    str = concat $ map serialize ss


instance Arbitrary Sequence where
    arbitrary = oneof [
        liftM TextSequence $
            listOf $ elements $ ['\0'..'\26'] ++ ['\28'..'\255'],
        liftM EscapeSequence2C $
            liftM (comb '\27') $ elements $ ['\64'..'\90'] ++ ['\92'..'\95'],
        EscapeSequence                             <$>
            arbitrary                              <*>
            (listOf $ arbitrary `suchThat` (>= 0)) <*>
            (listOf $ choose ('\32', '\47'))       <*>
            choose ('\64', '\126')]
        where comb a b = a:[b]
