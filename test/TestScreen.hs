module TestScreen where

import Test.QuickCheck

import Screen


main :: IO ()
main = do
    quickCheck prop_append
    quickCheck prop_update
    where
    fr = frame 3 2 6


prop_append :: Bool
prop_append =
    toStrings (append "h"           fr) == ["h\0\0", "\0\0\0"] &&
    toStrings (append "he"          fr) == ["he\0",  "\0\0\0"] &&
    toStrings (append "hel"         fr) == ["hel",   "\0\0\0"] &&
    toStrings (append "hell"        fr) == ["hel",   "l\0\0"]  &&
    toStrings (append "hello"       fr) == ["hel",   "lo\0"]   &&
    toStrings (append "hello "      fr) == ["hel",   "lo "]    &&
    toStrings (append "hello w"     fr) == ["lo ",   "w\0\0"]  &&
    toStrings (append "hello wo"    fr) == ["lo ",   "wo\0"]   &&
    toStrings (append "hello wor"   fr) == ["lo ",   "wor"]    &&
    toStrings (append "hello worl"  fr) == ["wor",   "l\0\0"]  &&
    toStrings (append "hello world" fr) == ["wor",   "ld\0"]   &&

    toStrings (append "hello world "        fr) == ["wor", "ld "]   &&
    toStrings (append "hello world x"       fr) == ["ld ", "x\0\0"] &&
    toStrings (append "hello world xy"      fr) == ["ld ", "xy\0"]  &&
    toStrings (append "hello world xyz"     fr) == ["ld ", "xyz"]   &&
    toStrings (append "hello world xyz "    fr) == ["xyz", " \0\0"] &&
    toStrings (append "hello world xyz u"   fr) == ["xyz", " u\0"]  &&
    toStrings (append "hello world xyz uv"  fr) == ["xyz", " uv"]   &&
    toStrings (append "hello world xyz uvw" fr) == [" uv", "w\0\0"] &&

    toStringsAll (append "hello world" fr) ==
        ["hel", "lo ", "wor", "ld\0"] &&
    toStringsAll (append "hello world xyz uvw" fr) ==
        ["ld ", "xyz", " uv", "w\0\0"]
    where
    fr = frame 3 2 4


prop_update :: Bool
prop_update =
    toStrings (update 1 1 'x' fr) == ["x\0\0",  "\0\0\0"] &&
    toStrings (update 1 2 'x' fr) == ["\0\0\0", "x\0\0"]  &&
    toStrings (update 2 1 'x' fr) == ["\0x\0",  "\0\0\0"] &&
    toStrings (update 2 2 'x' fr) == ["\0\0\0", "\0x\0"]  &&
    toStrings (update 3 1 'x' fr) == ["\0\0x",  "\0\0\0"] &&
    toStrings (update 3 2 'x' fr) == ["\0\0\0", "\0\0x"]
    where
    fr = frame 3 2 6
