-- Copyright (C) 2013 Che-Liang Chiou.

module StringFilter.Utils (
    getByte,
    getPrefix,
    matchByte,
    matchByteRange,
    matchInt,
    matchTable,
) where

import Control.Monad

import StringFilter


-- Reverse of (:)
uncons :: String -> FilterResult Char
uncons (c:cs) = Right (c, cs)
uncons _      = Left NeedMoreInput


getByte :: StringFilter Char
getByte = withInput uncons


getPrefix :: (Char -> Bool) -> StringFilter String
getPrefix pred = withInput $ getPrefix' pred
    where getPrefix' pred input =
            let result@(prefix, _) = span pred input
            in if null prefix
            then if null input
                 then Left NeedMoreInput
                 else Left NotMatch
            else Right result


matchByte :: Char -> StringFilter ()
matchByte expChar =
    getByte >>= \c ->
    if expChar == c
    then return ()
    else mzero


matchByteRange :: Char -> Char -> StringFilter Char
matchByteRange left right =
    getByte >>= \c ->
    if left <= c && c <= right
    then return c
    else mzero


matchInt :: StringFilter Int
matchInt = matchInt' [] where
    matchInt' digits =
        (matchByteRange '0' '9' >>= \d ->
         matchInt' (d:digits))
        `mplus`
        (if null digits
         then mzero
         else return $ read $ reverse digits)


matchTable :: [(Char, result)] -> StringFilter result
matchTable table =
    getByte >>= \c ->
    case lookup c table of
        Just result -> return result
        Nothing     -> mzero
