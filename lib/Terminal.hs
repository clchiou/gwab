-- Copyright (C) 2013 Che-Liang Chiou.

module Terminal (
    StringFilter.Error(Err, NeedMoreInput),

    Sequence(..),
    parse,
) where

import Control.Applicative
import Control.Monad

import StringFilter
import StringFilter.Utils


ansi_ESC = '\27'


data Sequence = TextSequence   { text    :: String }
              | EscapeSequence { control :: String }
                deriving (Eq, Show)


parse :: String -> FilterResult Sequence
parse = runFilter filterSequence


-- TODO: Single-character CSI '\155'
filterSequence :: StringFilter Sequence
filterSequence =
    -- Multi-character CSI
    (matchByte ansi_ESC >>
        (-- Two-character sequence
         (matchByteRange '\64' '\95' >>= \c ->
          (return $ EscapeSequence [c]))
         `mplus`
         -- Multi-character sequence
         (matchByte '\91' >>
          parseEscapeSequence)))
    `mplus`
    (getPrefix (/= ansi_ESC) >>=
     return . TextSequence)


parseEscapeSequence :: StringFilter Sequence
parseEscapeSequence = undefined
