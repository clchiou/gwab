-- Copyright (C) 2013 Che-Liang Chiou.

module StringFilter (
    StringFilter,
    runFilter,
    withInput
) where

import Control.Monad


data StringFilterState = StringFilterState {
    input :: String
} deriving (Show)


newtype StringFilter resultType = StringFilter {
    run :: StringFilterState -> Maybe (StringFilterState, resultType)
}


runFilter :: StringFilter resultType -> String -> Maybe (resultType, String)
runFilter packetFilter inputString =
    fmap massage (run packetFilter $ StringFilterState inputString)
    where massage (state, result) = (result, input state)


instance Monad StringFilter where
    return result  = StringFilter (\state -> Just (state, result))

    fail   message = StringFilter (\_ -> Nothing)

    filter0 >>= makeFilter1 = StringFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    Just (state1, result) -> run (makeFilter1 result) state1
                    Nothing               -> Nothing


instance MonadPlus StringFilter where
    mzero = StringFilter (\_ -> Nothing)

    mplus filter0 filter1 = StringFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    success@(Just _) -> success
                    Nothing          -> run filter1 state0


getState :: StringFilter StringFilterState
getState = StringFilter (\state -> Just (state, state))


putState :: StringFilterState -> StringFilter ()
putState state = StringFilter (\_ -> Just (state, ()))


withInput :: (String -> Maybe (resultType, String)) -> StringFilter resultType
withInput func =
    getState >>= \state ->
    case func $ input state of
        Just (result, rest) -> putState state{input=rest} >> return result
        Nothing             -> mzero
