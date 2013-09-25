-- Copyright (C) 2013 Che-Liang Chiou.

module Utils (
    getByte,
    matchByte,
    matchByteTable,
    span_ -- TODO: Remove this and use Regex
) where

import PacketFilter


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


getByte :: PacketFilter Char
getByte = withInput uncons


matchByte :: Char -> PacketFilter ()
matchByte expChar =
    getByte >>= \c ->
    if expChar == c
    then return ()
    else fail ("Expect " ++ [expChar] ++ " but got " ++ [c])


matchByteTable :: [(Char, result)] -> PacketFilter result
matchByteTable table =
    getByte >>= \c ->
    case lookup c table of
        Just result -> return result
        Nothing     -> fail ("Could not match " ++ [c])
