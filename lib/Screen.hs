-- Copyright (C) 2013 Che-Liang Chiou.

module Screen (
    Frame(..),
    frame,
    clear,
    append,
    update,
) where

import Data.Array.IArray


data Frame = Frame {
        width  :: Int,
        height :: Int,
        buffer :: FrameBuffer
    } deriving (Eq, Show)

-- TODO(clchiou): Use DiffArray?
type FrameBuffer = Array Int Line
type Line = Array Int Char


frame :: Int -> Int -> Frame
frame w h = Frame{width=w, height=h, buffer=listArray (1, h) lines}
    where lines = repeat $ listArray (1, w) $ repeat '\0'


clear :: Frame -> Frame
clear = uncurry frame . bounds . buffer


append :: String -> Frame -> Frame
append str f@(Frame w h _) = f{buffer=b''} where
    b'   = ixmap (1, h) (\j -> j `mod` h + 1) $ buffer f
    b''  = b' // [(h, toLine w str)]


update :: Int -> Int -> Char -> Frame -> Frame
update x y c f = f{buffer=buffer'} where
    buffer' = buffer f // [(y, line')]
    line'   = buffer f ! y // [(x, c)]


toLine :: Int -> String -> Line
toLine w str = listArray (1, w) str
