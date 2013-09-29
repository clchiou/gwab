-- Copyright (C) 2013 Che-Liang Chiou.

module StringFilter.Utils (
    getByte,
    getPrefix,
    matchByte,
    matchByteTable,
) where

import Control.Monad

import StringFilter


-- Reverse of (:)
uncons :: String -> Maybe (Char, String)
uncons (c:cs) = Just (c, cs)
uncons _      = Nothing


getByte :: StringFilter Char
getByte = withInput uncons


getPrefix :: (Char -> Bool) -> StringFilter String
getPrefix pred = withInput $ getPrefix' pred
    where getPrefix' pred seq =
            let result@(prefix, _) = span pred seq
            in if null prefix
            then Nothing
            else Just result


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
