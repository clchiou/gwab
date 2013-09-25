-- Copyright (C) 2013 Che-Liang Chiou.

module PacketFilter (
    PacketFilter,
    runFilter,
    withInput
) where

import Control.Monad


data PacketFilterState = PacketFilterState {
    input :: String
} deriving (Show)


newtype PacketFilter resultType = PacketFilter {
    run :: PacketFilterState -> Maybe (PacketFilterState, resultType)
}


runFilter :: PacketFilter resultType -> String -> Maybe (resultType, String)
runFilter packetFilter inputString =
    case run packetFilter (PacketFilterState inputString) of
        Just (state, result) -> Just (result, input state)
        Nothing              -> Nothing


instance Monad PacketFilter where
    return result  = PacketFilter (\state -> Just (state, result))

    fail   message = PacketFilter (\_ -> Nothing)

    filter0 >>= makeFilter1 = PacketFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    Just (state1, result) -> run (makeFilter1 result) state1
                    Nothing               -> Nothing


instance MonadPlus PacketFilter where
    mzero = PacketFilter (\_ -> Nothing)

    mplus filter0 filter1 = PacketFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    success@(Just _) -> success
                    Nothing          -> run filter1 state0


getState :: PacketFilter PacketFilterState
getState = PacketFilter (\state -> Just (state, state))


putState :: PacketFilterState -> PacketFilter ()
putState state = PacketFilter (\_ -> Just (state, ()))


withInput :: (String -> Maybe (resultType, String)) -> PacketFilter resultType
withInput func =
    getState >>= \state ->
    case func (input state) of
        Just (result, rest) ->
            putState state{input=rest} >>= \_ -> return result
        Nothing -> mzero
