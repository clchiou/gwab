module TestTelnet where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map (map)
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

    quickCheck prop_step_function


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

parse' str@(_:_) = packet : parse' rest where
    (packet, rest)               = unpack $ parse str
    unpack (Right result       ) = result
    unpack (Left  (Err reason) ) = error reason
    unpack (Left  NeedMoreInput) = error "Need more input"

parse' _ = []


serialize' :: [Packet] -> String
serialize' = concat . map serialize


identity :: String -> String
identity = serialize' . parse'


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
    arbitrary = fromList <$> listOf1 arbitrary


edge' = liftA2 edge
alter = (alter' <$>)
    where alter' (NvtOptBool   opt) = NvtOptBool $ not opt
          alter' (NvtOptAlways opt) = NvtOptAlways $ not opt
          alter' (NvtOptPair   opt) = NvtOptPair (1 + fst opt, snd opt)
          alter' (NvtOptString opt) = NvtOptString (opt ++ " ")
          alter' NvtOptNothing      = NvtOptNothing
makeZero (NvtContext table) = NvtContext $ zero where
    zero = Map.map (\_ -> NvtOptNothing) table


prop_absorbing_element :: Nvt -> Bool
prop_absorbing_element nvt =
    nvt  `edge'` zero == zero &&
    zero `edge'` nvt  == zero
    where zero = makeZero nvt


prop_zero_sum :: Nvt -> Bool
prop_zero_sum nvt = nvt `edge'` nvt == zero
    where zero = makeZero nvt


prop_edge_trigger :: Nvt -> Bool
prop_edge_trigger nvt =
    alter nvt `edge'`       nvt ==       nvt &&
          nvt `edge'` alter nvt == alter nvt


-- NOTE: When receiving WONT/DONT, DONT/WONT is the only valid response?
prop_step_function :: Bool
prop_step_function =
    step nvt PacketNop      == (nvt, []) &&
    step nvt PacketDataMark == (nvt, []) &&
    step nvt PacketBreak    == (nvt, []) &&
    step nvt PacketIp       == (nvt, []) &&
    step nvt PacketAo       == (nvt, []) &&
    step nvt PacketAyt      == (nvt, []) &&
    step nvt PacketEc       == (nvt, []) &&
    step nvt PacketEl       == (nvt, []) &&
    step nvt PacketGoAhead  == (nvt, []) &&

    step nvt (PacketWill '\0') == (         nvt, [PacketDo   '\0']) &&
    step nvt (PacketDo   '\0') == (         nvt, [PacketWill '\0']) &&
    step nvt (PacketWont '\0') == (u '\0' f nvt, [PacketDont '\0']) &&
    step nvt (PacketDont '\0') == (u '\0' f nvt, [PacketWont '\0']) &&

    step nvt (PacketWill '\1') == (u '\1' t nvt, [PacketDo   '\1']) &&
    step nvt (PacketDo   '\1') == (u '\1' t nvt, [PacketWill '\1']) &&
    step nvt (PacketWont '\1') == (         nvt, [PacketDont '\1']) &&
    step nvt (PacketDont '\1') == (         nvt, [PacketWont '\1']) &&

    step nvt (PacketWill '\3') == (nvt, [PacketDo   '\3']) &&
    step nvt (PacketDo   '\3') == (nvt, [PacketWill '\3']) &&
    step nvt (PacketWont '\3') == (nvt, [PacketDont '\3']) &&
    step nvt (PacketDont '\3') == (nvt, [PacketWont '\3']) &&

    step nvt' (PacketWill '\3') == (nvt', [PacketDont '\3']) &&
    step nvt' (PacketDo   '\3') == (nvt', [PacketWont '\3']) &&
    step nvt' (PacketWont '\3') == (nvt', [PacketDont '\3']) &&
    step nvt' (PacketDont '\3') == (nvt', [PacketWont '\3']) &&

    step nvt (PacketWill '\31') == (nvt, [PacketDo   '\31', windowSize]) &&
    step nvt (PacketDo   '\31') == (nvt, [PacketWill '\31', windowSize]) &&
    step nvt (PacketWont '\31') == (nvt, [PacketDont '\31']) &&
    step nvt (PacketDont '\31') == (nvt, [PacketWont '\31']) &&

    step nvt (PacketWill '\24') == (nvt, [PacketDo   '\24']) &&
    step nvt (PacketDo   '\24') == (nvt, [PacketWill '\24']) &&
    step nvt (PacketWont '\24') == (nvt, [PacketDont '\24']) &&
    step nvt (PacketDont '\24') == (nvt, [PacketWont '\24']) &&

    step nvt  (PacketSubOption "\24\1"      ) ==
        (nvt, [PacketSubOption "\24\0VT100"]) &&

    True
    where
    nvt = fromList
        [(rfc856_BINARY_TRANSMISSION, NvtOptBool True),
         (rfc857_ECHO,                NvtOptBool False),
         (rfc858_SUPPRESS_GOAHEAD,    NvtOptAlways True),
         (rfc1073_WINDOW_SIZE,        NvtOptPair (80, 24)),
         (rfc1091_TERMINAL_TYPE,      NvtOptString "VT100")]

    nvt' = u '\3' (NvtOptAlways False) nvt

    u = update
    t = NvtOptBool True
    f = NvtOptBool False
    windowSize = PacketSubOption "\31\00\80\00\24"
