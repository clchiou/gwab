-- Copyright (C) 2013 Che-Liang Chiou.

module Screen (
    Frame(..),

    frame,

    goto,
    scroll,

    append,
    write,
    update,

    -- Exported for testing purposes
    toStrings,
    toStringsAll,
) where

import Data.Array.IArray

import Platform (join)

import Screen.Utils


data Frame = Frame {
    dimension :: (Int, Int, Int),
    cursor    :: (Int, Int, Int),
    buffer    :: FrameBuffer
} deriving (Eq, Show)

-- TODO(clchiou): Use DiffArray?
type FrameBuffer = Array Int FrameLine
type FrameLine   = Array Int Dot

data Dot = DotChar Char
         | DotSpan
         | DotEmpty
           deriving (Eq, Show)


frame :: Int -> Int -> Int -> Frame
frame w h l = Frame {
        dimension = (w, h, l),
        cursor    = (1, 1, 0),
        buffer    = listArray (1, l) lines
    } where lines = repeat (frameLine w)


goto :: Int -> Int -> Frame -> Frame
goto x y fr@(Frame _ (_, _, s) _) = fr{cursor=(x, y, s)}


scroll :: Int -> Frame -> Frame
scroll n fr@(Frame (w, h, l) (x, y, s) buf)
    | n == 0 = fr
    | n <  0 = -- Scroll up
        if s + n > 0
        then fr{cursor=(x, y, s + n)}
        else fr{cursor=(x, y, 1)}
    | n >  0 = -- Scroll down
        if s + h + n < l
        then fr{cursor=(x, y, s + n)}
        else scroll n fr{cursor=(x, y, s - 1), buffer=buf'}
    where
    buf' = scroll1 w l buf


append :: String -> Frame -> Frame
append str fr@(Frame _ (x, y, _) _) = write x y str fr


write :: Int -> Int -> String -> Frame -> Frame
write x y str fr = write' x y (fromString str) fr


-- TODO(clchiou): Handle case that CJK code point span over line boundary
write' :: Int -> Int -> [Dot] -> Frame -> Frame
write' x y [] fr = goto x y fr
write' x y dots fr@(Frame (w, h, l) (_, _, s) buf)
    | x + length dots <= w + 1 =
        fr{cursor=(x + length dots, y, s),
           buffer=writeBuffer x (s + y) dots buf}
    | y < h =
        write' 1 (y + 1) rest fr{buffer=buf'}
    | s + h < l =
        write' 1 y rest fr{cursor=(x, y, s + 1), buffer=buf'}
    | otherwise =
        write' 1 y rest fr{buffer=buf''}
    where
    (dots', rest) = splitAt (w - x + 1) dots
    buf'  = writeBuffer x (s + y) dots' buf
    buf'' = scroll1 w l buf'


writeBuffer :: Int -> Int -> [Dot] -> FrameBuffer -> FrameBuffer
writeBuffer x y dots buf = buf // [(y, line)] where
    line = buf ! y // zip (iterate (+1) x) dots


scroll1 :: Int -> Int -> FrameBuffer -> FrameBuffer
scroll1 w l buf = ixmap (1, l) (\j -> j `mod` l + 1) buf // [(l, frameLine w)]


update :: Int -> Int -> Char -> Frame -> Frame
update x y c fr@(Frame _ (_, _, s) buf) = fr{buffer=buf'} where
    buf'  = buf // [(s + y, line')]
    line' = buf ! (s + y) // [(x, fromChar c)]


frameLine :: Int -> FrameLine
frameLine w = listArray (1, w) $ repeat DotEmpty


fromChar :: Char -> Dot
fromChar = DotChar


fromString :: String -> [Dot]
fromString (c:cs)
    | isCjkCodePoint c = [DotChar c, DotSpan] ++ fromString cs
    | otherwise        = DotChar c : fromString cs
fromString _           = []


toChar :: Dot -> Char
toChar (DotChar c) = c
toChar DotSpan     = '\0'
toChar DotEmpty    = '\0'


toString :: FrameLine -> String
toString = map toChar . filter (not . isDotSpan) . elems where
    isDotSpan DotSpan = True
    isDotSpan _       = False


toStrings :: Frame -> [String]
toStrings fr@(Frame (w, h, _) (_, _, s) buf) =
    map toString $ map (buf !) [(s + 1) .. (s + h)]


toStringsAll :: Frame -> [String]
toStringsAll = map toString . elems . buffer
