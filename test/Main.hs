module Main where

import Control.Monad
import Data.String.Utils
import Test.QuickCheck

import Telnet


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


main :: IO ()
main = do
    quickCheck prop_idempotent1
    quickCheck prop_idempotent2


prop_idempotent1 :: [Char] -> Bool
prop_idempotent1 cs = (concat . map serialize . parse) quoted == quoted
    where quoted = quote cs
          quote  = replace "\255" "\255\255" -- This quotes all IAC


-- NOTE: Test idempotency for [Packet] is difficult because:
--   * [PacketText ""] is serialized to "" but "" is parsed to [] rather than
--     [PacketText ""].
--   * PacketText may be generated at different splits of input text.
-- So the easies way to test idempotency is to test on serialized text.
prop_idempotent2 :: [Packet] -> Bool
prop_idempotent2 ps =
    let cs  = concat $ map serialize ps
        ps' = parse cs
        cs' = concat $ map serialize ps'
    in cs == cs'
