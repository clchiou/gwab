-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet.Internal.Utils where

import Data.Char

import Platform (replace)

import Telnet.Consts


-- NOTE: If you add fields to NvtContext, DO NOT forget to add it to
-- foldlN and foldl1N also!
foldlN :: (a -> b -> a) -> a -> NvtContext b -> a
foldlN f z nvt =
    z              `f`
    binary     nvt `f`
    echo       nvt `f`
    supGoAhead nvt `f`
    windowSize nvt


foldl1N :: (a -> a -> a) -> NvtContext a -> a
foldl1N f nvt =
    binary     nvt `f`
    echo       nvt `f`
    supGoAhead nvt `f`
    windowSize nvt


ack :: Packet -> Packet
ack (PacketWill opt) = PacketDo   opt
ack (PacketDo   opt) = PacketWill opt
ack (PacketWont opt) = PacketDont opt
ack (PacketDont opt) = PacketWont opt


nak :: Packet -> Packet
nak (PacketWill opt) = PacketDont opt
nak (PacketDo   opt) = PacketWont opt
nak (PacketWont opt) = PacketDont opt
nak (PacketDont opt) = PacketWont opt


-- NAWS : Negotiate About Window Size
naws :: Int -> Int -> Packet
naws width height = PacketSubOption subopt
    where subopt  = rfc1073_WINDOW_SIZE : width' ++ height'
          width'  = serialize_be16 width
          height' = serialize_be16 height


serialize_be16 :: Int -> String
serialize_be16 x = [chr hi, chr lo]
    where (hi, lo) = divMod x 256


quote :: String -> String
quote = replace [rfc854_IAC] [rfc854_IAC, rfc854_IAC]


-- NOTE: readUntilIac is NOT lazy; it continues to read until it sees an IAC
-- that is not sent as data, i.e., start of next command.  The non-laziness is
-- probably only useful when parsing subnegotiation where the suboption is not
-- complete until we see an SE command.
--
-- This function also unquotes IACs that are sent as data.
readUntilIac :: String -> String -> Maybe (String, String)
readUntilIac prefix suffix
    | null suffix =
        if null prefix
        then Nothing
        else Just (prefix, suffix)
    | otherwise =
        if iacSentAsData == [rfc854_IAC, rfc854_IAC]
        then readUntilIac (prefix ++ prefix' ++ [rfc854_IAC]) suffix''
        else Just (prefix ++ prefix', suffix')
        where (prefix', suffix')        = span (/= rfc854_IAC) suffix
              (iacSentAsData, suffix'') = splitAt 2 suffix'
