module Main where

import Control.Applicative
import Control.Monad
import Data.String.Utils
import Test.QuickCheck

import Telnet
import Telnet.Utils


main :: IO ()
main = do
    quickCheck prop_need_more_input
    quickCheck prop_erroneous_command
    quickCheck prop_erroneous_subnegotiation

    quickCheck prop_identity1
    quickCheck prop_identity2

    quickCheck prop_absorbing_element
    quickCheck prop_zero_sum
    quickCheck prop_edge_trigger


--
-- Tests of packet parse/serialize
--


prop_need_more_input :: Bool
prop_need_more_input = all ((Left NeedMoreInput ==) . parse) [
    "\255",
    "\255\250",
    "\255\250\255",
    "\255\250abc",
    "\255\250abc\255",
    "\255\251",
    "\255\252",
    "\255\253",
    "\255\254",
    ""]


prop_erroneous_command :: Bool
prop_erroneous_command = all test ['\0'..'\239'] where
    test = isError . parse . ("\255" `sonc`)


prop_erroneous_subnegotiation :: Bool
prop_erroneous_subnegotiation = all (isError . parse) [
    "\255\250\255\1",
    "\255\250abc\255\1"]


isError result =
    case result of
        (Left (Err _)) -> True
        _              -> False


sonc :: String -> Char -> String
sonc cs c = cs ++ [c]


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


parse' :: String -> [Packet]
parse' str@(_:_) = packet : parse' rest
    where (packet, rest)               = unpack $ parse str
          unpack (Right result       ) = result
          unpack (Left  (Err reason) ) = error reason
          unpack (Left  NeedMoreInput) = error "Need more input"
parse' _ = []

serialize' :: [Packet] -> String
serialize' = concat . map serialize

identity   = serialize' . parse'


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


instance Arbitrary NvtOpt where
    arbitrary = oneof [
        liftM  NvtOptBool   arbitrary,
        liftM  NvtOptAlways arbitrary,
        liftM  NvtOptPair   arbitrary,
        liftM  NvtOptString arbitrary,
        return NvtOptNothing]


instance Arbitrary a => Arbitrary (NvtContext a) where
    arbitrary = NvtContext <$> arbitrary <*>
                               arbitrary <*>
                               arbitrary <*>
                               arbitrary <*>
                               arbitrary


edge' = liftA2 edge
zero  = pure NvtOptNothing
alter = liftA alter'
    where alter' (NvtOptBool   opt) = NvtOptBool $ not opt
          alter' (NvtOptAlways opt) = NvtOptAlways $ not opt
          alter' (NvtOptPair   opt) = NvtOptPair (1 + fst opt, snd opt)
          alter' (NvtOptString opt) = NvtOptString (opt ++ " ")
          alter' NvtOptNothing      = NvtOptNothing


prop_absorbing_element :: Nvt -> Bool
prop_absorbing_element nvt =
    nvt  `edge'` zero == zero &&
    zero `edge'` nvt  == zero


prop_zero_sum :: Nvt -> Bool
prop_zero_sum nvt = nvt `edge'` nvt == zero


prop_edge_trigger :: Nvt -> Bool
prop_edge_trigger nvt =
    alter nvt `edge'`       nvt ==       nvt &&
          nvt `edge'` alter nvt == alter nvt
