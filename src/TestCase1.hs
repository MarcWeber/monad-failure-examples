{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts,
    MultiParamTypeClasses, OverlappingInstances, FlexibleInstances,
    NoMonomorphismRestriction #-}

-- this options pipes this file through a preprocessor calling the withLoc
-- wrapping all monadic actions by a withLoc "source location" call.
-- the withLoc is a "no operation" implementation if the monad doesn't support
-- error locations. Thus you can enable/disable this feature by choosing a different monad type.
-- Have a look at the monadloc package on hackage to get to know more details
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module TestCase1 where
import Control.Exception hiding ( throw, DivideByZero )
import Control.Monad.Exception.Base (throw)
import Control.Monad.Loc
import Data.Typeable

{- ==== the testcase: ==========
  * some recursion depth in order to test traces
  * You can use a monad supporting source locations
-}

-- Note: Control.Monad.Exception.Throws explains how to use subexceptions forcing you
--       declaring Throws instances manually.

data IsNull = IsNull deriving (Show, Typeable)
data TooLow = TooLow deriving (Show, Typeable)


instance Exception TooLow where
    toException se = SomeException se
    fromException x = fromException x

instance Exception IsNull where
    toException se = SomeException se
    fromException x = fromException x

-- intentionally using "do throw .." 
-- do will add the final location information onto the CallTrace
recurse _   n _ | n <= -100 = do throw TooLow
recurse _   (-1)   _ = return "ok"
recurse act (-2)   _ = act
recurse _      0   _ = do throw IsNull
recurse act   n f = f act (n -1)

a act n = do recurse act n b
b act n = do recurse act n a

-- act: arbitrary value or monad action 
-- nr: recursion depth
-- nr = -1: return "ok"
-- nr = -2: run act
-- nr >= 0: throw Exception IsNull
testCase1 act nr = do recurse act nr a
