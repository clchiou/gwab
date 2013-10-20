module TestScreen where

import Test.QuickCheck

import Screen


main :: IO ()
main = do
    quickCheck prop_append
    quickCheck prop_update


prop_append :: Bool
prop_append =
    toStrings (append "h"           fr) == ["h\0\0", "\0\0\0"] &&
    toStrings (append "he"          fr) == ["he\0",  "\0\0\0"] &&
    toStrings (append "hel"         fr) == ["hel",   "\0\0\0"] &&
    toStrings (append "hell"        fr) == ["hel",   "l\0\0"]  &&
    toStrings (append "hello"       fr) == ["hel",   "lo\0"]   &&
    toStrings (append "hello "      fr) == ["hel",   "lo "]    &&
    toStrings (append "hello w"     fr) == ["wel",   "lo "]    &&
    toStrings (append "hello wo"    fr) == ["wol",   "lo "]    &&
    toStrings (append "hello wor"   fr) == ["wor",   "lo "]    &&
    toStrings (append "hello worl"  fr) == ["wor",   "lo "]    &&
    toStrings (append "hello world" fr) == ["wor",   "ld "]
    where
    fr = frame 3 2


prop_update :: Bool
prop_update =
    toStrings (update 1 1 'x' fr) == ["x\0\0",  "\0\0\0"] &&
    toStrings (update 1 2 'x' fr) == ["\0\0\0", "x\0\0"]  &&
    toStrings (update 2 1 'x' fr) == ["\0x\0",  "\0\0\0"] &&
    toStrings (update 2 2 'x' fr) == ["\0\0\0", "\0x\0"]  &&
    toStrings (update 3 1 'x' fr) == ["\0\0x",  "\0\0\0"] &&
    toStrings (update 3 2 'x' fr) == ["\0\0\0", "\0\0x"]
    where
    fr = frame 3 2
