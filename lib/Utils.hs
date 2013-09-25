-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    getByte,
    matchByte,
    matchByteTable,
    span_ -- TODO: Remove this and use Regex
) where

import Control.Monad

import StringFilter


-- Reverse of (:)
uncons :: String -> Maybe (Char, String)
uncons (c:cs) = Just (c, cs)
uncons _      = Nothing


-- Maybe version of span
span_ :: (a -> Bool) -> [a] -> Maybe ([a], [a])
span_ pred seq =
    let (prefix, suffix) = span pred seq
    in if null prefix
    then Nothing
    else Just (prefix, suffix)


getByte :: StringFilter Char
getByte = withInput uncons


matchByte :: Char -> StringFilter ()
matchByte expChar =
    getByte >>= \c ->
    if expChar == c
    then return ()
    else mzero


matchByteTable :: [(Char, result)] -> StringFilter result
matchByteTable table =
    getByte >>= \c ->
    case lookup c table of
        Just result -> return result
        Nothing     -> mzero
