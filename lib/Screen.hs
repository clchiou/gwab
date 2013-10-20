-- Copyright (C) 2013 Che-Liang Chiou.

module Screen (
    Frame(..),

    frame,
    clear,

    append,
    write,
    update,

    toStrings,
) where

import Data.Array.IArray

import Platform (join)


data Frame = Frame {
    dimension :: (Int, Int),
    cursor    :: (Int, Int),
    buffer    :: FrameBuffer
} deriving (Eq, Show)

-- TODO(clchiou): Use DiffArray?
type FrameBuffer = Array Int Line
type Line = Array Int Char


frame :: Int -> Int -> Frame
frame w h = Frame {
        dimension = (w, h),
        cursor    = (1, 1),
        buffer    = listArray (1, h) lines
    } where lines = repeat $ listArray (1, w) $ repeat '\0'


clear :: Frame -> Frame
clear = uncurry frame . bounds . buffer


append :: String -> Frame -> Frame
append str fr@(Frame _ (x, y) _) = write x y str fr


write :: Int -> Int -> String -> Frame -> Frame

write x y [] fr = fr{cursor=(x, y)}

write x y str fr@(Frame (w, h) _ buf)
    | x + length str < w + 1 =
        fr{cursor=(x + length str, y), buffer=write' x y str buf}
    | otherwise =
        write 1 y' rest fr{buffer=write' x y str' buf}
    where
    y' = y `mod` h + 1
    (str', rest) = splitAt (w - x + 1) str


write' :: Int -> Int -> String -> FrameBuffer -> FrameBuffer
write' x y str buf = buf // [(y, line)] where
    line = buf ! y // zip (iterate (+1) x) str


update :: Int -> Int -> Char -> Frame -> Frame
update x y c f = f{buffer=buffer'} where
    buffer' = buffer f // [(y, line')]
    line'   = buffer f ! y // [(x, c)]


toStrings :: Frame -> [String]
toStrings fr = map elems $ elems $ buffer fr
