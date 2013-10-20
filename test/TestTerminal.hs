module TestTerminal where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Test.QuickCheck

import Terminal


main :: IO ()
main = do
    quickCheck prop_c0_control_code
    quickCheck prop_c1_control_code
    quickCheck prop_escape_sequence
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


prop_c0_control_code :: Bool
prop_c0_control_code = all test ['\0'..'\31'] where
    test c = parse' [c] == [ControlCode [c]]


prop_c1_control_code :: Bool
prop_c1_control_code = all test (delete '\91' ['\64'..'\95']) where
    test c = parse' ['\27', c] == [ControlCode ['\27', c]]


prop_escape_sequence :: Bool
prop_escape_sequence = (serialize' $ parse' "\ESC[;33m") == "\ESC[33m"


prop_text_sequence :: PrintableString -> Bool
prop_text_sequence (PrintableString str) =
    parse' str == [TextSequence str]


prop_identity :: [Sequence] -> Bool
prop_identity ss = identity str == str where
    str = concat $ map serialize ss


newtype PrintableString = PrintableString String
                          deriving (Show)


instance Arbitrary PrintableString where
    arbitrary = fmap PrintableString $
                listOf1 $ elements $ filter isPrint ['\0'..'\255']


instance Arbitrary Sequence where
    arbitrary = oneof [
        liftM TextSequence $
            listOf $ elements $ delete '\27' ['\0'..'\255'],
        liftM ControlCode $
            liftM (:[]) $ elements ['\0'..'\31'],
        liftM ControlCode $
            liftM (('\27':) . singleton) $
                  elements $ delete '\91' ['\64'..'\95'],
        EscapeSequence                             <$>
            arbitrary                              <*>
            (listOf $ arbitrary `suchThat` (>= 0)) <*>
            (listOf $ choose ('\32', '\47'))       <*>
            choose ('\64', '\126')]


singleton :: a -> [a]
singleton a = [a]
