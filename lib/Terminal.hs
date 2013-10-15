-- Copyright (C) 2013 Che-Liang Chiou.

module Terminal (
    StringFilter.Error(Err, NeedMoreInput),

    Sequence(..),
    parse,
    serialize,
) where

import Control.Monad (mplus)
import Data.List.Utils (join)

import StringFilter
import StringFilter.Utils


ansi_ESC = '\27'
ansi_CSI = '\91'


data Sequence = TextSequence String
              | EscapeSequence {
                    privateMode   :: Bool,
                    parameters    :: [Int],
                    intermediates :: String,
                    letter        :: Char
                }
              | EscapeSequence2C String
                deriving (Eq, Show)


parse :: String -> FilterResult Sequence
parse = runFilter filterSequence


serialize :: Sequence -> String
serialize (TextSequence     text   ) = text
serialize (EscapeSequence2C command) = command
serialize escape =
    [ansi_ESC, ansi_CSI] ++
    privateMode'         ++
    parameters'          ++
    intermediates'       ++
    letter'
    where
    privateMode'   = if privateMode escape then "?" else ""
    parameters'    = join ";" $ map show $ parameters escape
    intermediates' = intermediates escape
    letter'        = [letter escape]


-- TODO: Single-character CSI '\155'
filterSequence :: StringFilter Sequence
filterSequence =
    -- Multi-character CSI
    (matchByte (== ansi_ESC) >>
        (-- Multi-character sequence
         (matchByte (== ansi_CSI) >>
          parseEscapeSequence)
         `mplus`
         -- Two-character sequence
         (matchByte (isRange '\64' '\95') >>= \c ->
          (return $ EscapeSequence2C [ansi_ESC, c]))
         `mplus`
         (fail $ "Could not parse escape sequence")))
    `mplus`
    (getPrefix (/= ansi_ESC) >>=
     return . TextSequence)


parseEscapeSequence :: StringFilter Sequence
parseEscapeSequence =
    (matchByte (== '?') >> parseParameter True)
    `mplus`
    (parseParameter False)


parseParameter :: Bool -> StringFilter Sequence
parseParameter privateMode =
    (matchInt >>= \parameter ->
     parseParameters privateMode [parameter])
    `mplus`
    (parseIntermediate privateMode [] [])


parseParameters :: Bool -> [Int] -> StringFilter Sequence
parseParameters privateMode parameters =
    (matchByte (== ';') >>
     matchInt >>= \parameter ->
     parseParameters privateMode (parameters ++ [parameter]))
    `mplus`
    (parseIntermediate privateMode parameters [])


parseIntermediate :: Bool -> [Int] -> String -> StringFilter Sequence
parseIntermediate privateMode parameters intermediates =
    (matchByte (isRange '\32' '\47') >>= \intermediate ->
     parseIntermediate privateMode parameters (intermediates ++ [intermediate]))
    `mplus`
    (parseLetter privateMode parameters intermediates)


parseLetter :: Bool -> [Int] -> String -> StringFilter Sequence
parseLetter privateMode parameters intermediates =
    (matchByte (isRange '\64' '\126') >>= \letter ->
     return EscapeSequence {
        privateMode   = privateMode,
        parameters    = parameters,
        intermediates = intermediates,
        letter        = letter})


isRange :: Char -> Char -> Char -> Bool
isRange left right c = left <= c && c <= right
