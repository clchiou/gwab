-- Copyright (C) 2013 Che-Liang Chiou.

module StringFilter (
    Error(..),
    FilterResult,
    StringFilter,
    runFilter,
    withInput,
) where

import Control.Monad


newtype StringFilter resultType = StringFilter {
    run :: StringFilterState -> Either Error (StringFilterState, resultType)
}


newtype StringFilterState = StringFilterState {
    input :: String
} deriving (Show)


data Error = Err String
           | NeedMoreInput
           | NotMatch
             deriving (Eq, Show)


type FilterResult resultType = Either Error (resultType, String)


runFilter :: StringFilter resultType -> String -> FilterResult resultType
runFilter packetFilter inputString =
    fmap massage (run packetFilter $ StringFilterState inputString)
    where massage (state, result) = (result, input state)


instance Monad StringFilter where
    return result  = StringFilter (\state -> Right (state, result))

    fail   message = StringFilter (\_     -> Left  (Err message))

    filter0 >>= makeFilter1 = StringFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    Right (state1, result) -> run (makeFilter1 result) state1
                    Left  (Err reason)     -> Left (Err reason)
                    Left  NeedMoreInput    -> Left NeedMoreInput
                    Left  NotMatch         -> Left NotMatch


instance MonadPlus StringFilter where
    mzero = makeZero NotMatch

    mplus filter0 filter1 = StringFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    success@(Right _)  -> success
                    Left (Err reason)  -> Left (Err reason)
                    Left NeedMoreInput -> Left NeedMoreInput
                    Left NotMatch      -> run filter1 state0


makeZero :: Error -> StringFilter resultType
makeZero e = StringFilter (\_ -> Left e)


getState :: StringFilter StringFilterState
getState = StringFilter (\state -> Right (state, state))


putState :: StringFilterState -> StringFilter ()
putState state = StringFilter (\_ -> Right (state, ()))


withInput :: (String -> FilterResult resultType) -> StringFilter resultType
withInput func =
    getState >>= \state ->
    case func $ input state of
        Right (result, rest) -> putState state{input=rest} >> return result
        Left  (Err reason)   -> fail reason
        Left  NeedMoreInput  -> makeZero NeedMoreInput
        Left  NotMatch       -> makeZero NotMatch
