module Main where

import Control.Applicative
import Control.Monad
import Data.String.Utils
import Test.QuickCheck

import Telnet


main :: IO ()
main = do
    quickCheck prop_identity1
    quickCheck prop_identity2

    quickCheck prop_absorbing_element
    quickCheck prop_zero_sum
    quickCheck prop_edge_trigger


--
-- Tests of packet parse/serialize
--


instance Arbitrary Packet where
    arbitrary = oneof [
        return PacketNop,
        return PacketDataMark,
        return PacketBreak,
        return PacketIp,
        return PacketAo,
        return PacketAyt,
        return PacketEc,
        return PacketEl,
        return PacketGoAhead,
        liftM  PacketSubOption arbitrary,
        liftM  PacketWill      arbitrary,
        liftM  PacketWont      arbitrary,
        liftM  PacketDo        arbitrary,
        liftM  PacketDont      arbitrary,
        liftM  PacketText      arbitrary]


serialize' = concat . map serialize
identity   = serialize' . parse


prop_identity1 :: [Char] -> Bool
prop_identity1 cs = identity quoted == quoted
    where quoted = quote cs
          quote  = replace "\255" "\255\255" -- This quotes all IAC


-- NOTE: Test identity for [Packet] is difficult because:
--   * [PacketText ""] is serialized to "" but "" is parsed to [] rather than
--     [PacketText ""].
--   * PacketText may be generated at different splits of input text.
-- So the easies way to test identity is to test on serialized text.
prop_identity2 :: [Packet] -> Bool
prop_identity2 ps = identity cs == cs
    where cs = serialize' ps


--
-- Tests of NvtContext
--


instance Arbitrary a => Arbitrary (NvtContext a) where
    arbitrary = liftM3 NvtContext arbitrary arbitrary arbitrary


edgeTrigger' = liftA2 edgeTrigger
not'         = liftA (liftA not)
zero         = pure Nothing


prop_absorbing_element :: NvtContext (Maybe Bool) -> Bool
prop_absorbing_element nvt =
    nvt  `edgeTrigger'` zero == zero &&
    zero `edgeTrigger'` nvt  == zero


prop_zero_sum :: NvtContext (Maybe Bool) -> Bool
prop_zero_sum nvt = nvt `edgeTrigger'` nvt == zero


prop_edge_trigger :: NvtContext (Maybe Bool) -> Bool
prop_edge_trigger nvt =
    nvt      `edgeTrigger'` not' nvt == not' nvt &&
    not' nvt `edgeTrigger'`      nvt ==      nvt
