-- Copyright (C) 2013 Che-Liang Chiou.

module Screen.Utils (
    isCjkCodePoint,
) where


-- Chapter 12, East Asian Scripts, The Unicode Standard, Version 5.0.
-- Table 12-2. Blocks Containing Han Ideographs
isCjkCodePoint :: Char -> Bool
isCjkCodePoint c =
    '\x4e00'  <= c && c <= '\x9fff'  ||
    '\x3400'  <= c && c <= '\x4dff'  ||
    '\x20000' <= c && c <= '\x2a6df' ||
    '\xf900'  <= c && c <= '\xfaff'  ||
    '\x2f800' <= c && c <= '\x2fa1f'
