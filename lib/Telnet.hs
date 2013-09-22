-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet where

import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.Char as Char (chr, ord)


telnetSe        = Char.chr 240 -- End of subnegotiation
telnetNop       = Char.chr 241 -- NOP
telnetDataMark  = Char.chr 242 -- Data Mark
telnetBreak     = Char.chr 243 -- Break
telnetIp        = Char.chr 244 -- Interrupt Process
telnetAo        = Char.chr 245 -- Abort output
telnetAyt       = Char.chr 246 -- Are you there
telnetEc        = Char.chr 247 -- Erase character
telnetEl        = Char.chr 248 -- Erase line
telnetGoAhead   = Char.chr 249 -- Go ahead
telnetSb        = Char.chr 250 -- Begin of subnegotiation
telnetWill      = Char.chr 251 -- WILL
telnetWont      = Char.chr 252 -- WON'T
telnetDo        = Char.chr 253 -- DO
telnetDont      = Char.chr 254 -- DON'T
telnetIac       = Char.chr 255 -- IAC


data TelnetPacket = TelnetText { getText :: String }
                  | TelnetSe
                  | TelnetNop
                  | TelnetDataMark
                  | TelnetBreak
                  | TelnetIp
                  | TelnetAo
                  | TelnetAyt
                  | TelnetEc
                  | TelnetEl
                  | TelnetGoAhead
                  | TelnetSb
                  | TelnetWill  { getOption :: Int }
                  | TelnetWont  { getOption :: Int }
                  | TelnetDo    { getOption :: Int }
                  | TelnetDont  { getOption :: Int }
                    deriving (Show)


telnetRecv :: [ByteString] -> [TelnetPacket]
telnetRecv (b:bs)
    | Char8.null b = telnetRecv bs
    | otherwise    =
        let (packets, b') = telnetParse b
        in packets ++ telnetRecvRemainder b' bs
telnetRecv _ = []


telnetRecvRemainder :: ByteString -> [ByteString] -> [TelnetPacket]
telnetRecvRemainder r bs_input@(b:bs)
    | Char8.null r = telnetRecv bs_input
    | otherwise    = telnetRecv $ (Char8.append r b):bs
telnetRecvRemainder _ _ = [] -- TODO: If r != null, this is illegal...


-- TODO: Use regular expression...
telnetParse :: ByteString -> ([TelnetPacket], ByteString)
telnetParse b
    | Char8.null b = ([], Char8.empty)
    | otherwise    =
        let (text, command) = Char8.span (/= telnetIac) b
            pair@(ps, b')   = telnetParseCommand command
        in if Char8.null text
           then pair
           else ((TelnetText $ Char8.unpack text):ps, b')


telnetParseCommand :: ByteString -> ([TelnetPacket], ByteString)
telnetParseCommand b
    | Char8.null b       = ([], Char8.empty)
    | Char8.length b < 2 = ([], b)
    | otherwise          =
        let b'         = Char8.tail b
            command    = Char8.head b'
            (mp, b'')  = telnetParseCommand' command (Char8.tail b')
            (ps, b''') = telnetParse b''
        in case mp of
            Just p  -> (p:ps, b''')
            Nothing -> (ps, b''')


telnetParseCommand' :: Char -> ByteString -> (Maybe TelnetPacket, ByteString)
telnetParseCommand' command b
    | command == telnetSe       = (Just TelnetSe, b)
    | command == telnetNop      = (Just TelnetNop, b)
    | command == telnetDataMark = (Just TelnetDataMark, b)
    | command == telnetBreak    = (Just TelnetBreak, b)
    | command == telnetIp       = (Just TelnetIp, b)
    | command == telnetAo       = (Just TelnetAo, b)
    | command == telnetAyt      = (Just TelnetAyt, b)
    | command == telnetEc       = (Just TelnetEc, b)
    | command == telnetEl       = (Just TelnetEl, b)
    | command == telnetGoAhead  = (Just TelnetGoAhead, b)
    | command == telnetSb       = (Just TelnetSb, b)
-- Negotiations
    | command == telnetWill     =
        if Char8.null b
        then (Nothing, Char8.singleton command)
        else (Just $ TelnetWill $ Char.ord $ Char8.head b, Char8.tail b)
    | command == telnetWont     =
        if Char8.null b
        then (Nothing, Char8.singleton command)
        else (Just $ TelnetWont $ Char.ord $ Char8.head b, Char8.tail b)
    | command == telnetDo       =
        if Char8.null b
        then (Nothing, Char8.singleton command)
        else (Just $ TelnetDo   $ Char.ord $ Char8.head b, Char8.tail b)
    | command == telnetDont     =
        if Char8.null b
        then (Nothing, Char8.singleton command)
        else (Just $ TelnetDont $ Char.ord $ Char8.head b, Char8.tail b)
-- IAC sent as text
    | command == telnetIac      = (Just $ TelnetText [telnetIac], b)
telnetParseCommand' _ _ = error "Could not parse command code"


toByteString :: TelnetPacket -> ByteString
toByteString p = case p of
    TelnetText text -> Char8.pack text
    TelnetWont opt  -> Char8.cons telnetIac
                                  (Char8.cons telnetWont
                                              (Char8.singleton $ Char.chr opt))
    TelnetDont opt  -> Char8.cons telnetIac
                                  (Char8.cons telnetDont
                                              (Char8.singleton $ Char.chr opt))
    otherwise       -> undefined
