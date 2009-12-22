{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}
module Main where
import Control.Monad.Exception.Throws
import Control.Monad
import Data.Function
import Control.Exception
import Control.Monad.Exception.Base
import Test.HUnit
import TestCase1

-- DOC: read Control.Monad.Exception to get an overview 

-- Don't know how to implement instance Show (Either SomeException a), so use show to compare strings
eqShow = (~?=)`on` show
nop = return "nop"


-- c-m-e (control-monad-exception):
monadFailureTests = TestList [

    -- the EMT monad contains the either the Exception or the result value.
    -- It's very similar to the Error Monad. It adds a phantom type keeping track
    -- about which Exceptions are thrown. Eg Java is using a throws declaration
    -- to do the same. Using monad-failure you don't have to type this declaration
    -- if you don't want to, ghc will infer it.
    
    -- Using Identity monad because the framework must be usable using arbitrary monads as well
    "tryEMT ok"        ~:           (runIdentity . tryEMT . (testCase1 nop )) (-1) `eqShow` Right "ok"

    -- there is a shortcut: tryEM = runIdentity . tryEMT
  , "tryEM Exception"  ~:           (              tryEM  . (testCase1 nop)) 40   `eqShow` Left (toException IsNull)

  ]

tryEMTWithLoc :: Monad m => EMT (AnyException l) m a -> m (Either (CallTrace, SomeException) a)
tryEMTWithLoc = liftM (mapLeft (\(l,ce) -> (l, checkedException ce))) . unEMT

tryEMWithLoc :: EM (AnyException l) a -> Either (CallTrace, SomeException) a
tryEMWithLoc = runIdentity . tryEMTWithLoc

-- (control-monad- 
monadFailureWithErrorLocationsTest = TestList [
  -- after catching there is no Exception left which can be caught.
  -- Thus the outer Either should always be a Right value

    "tryEM with loc ok"        ~:           ( tryEMWithLoc . (testCase1 nop)) (-1) `eqShow` (Right "ok")

  , "tryEM with loc Exception" ~:           ( tryEMWithLoc . (testCase1 nop))   3  `eqShow` (Left (trace, toException IsNull))

  ]

  where trace = [
                  "testCase1, TestCase1(src/TestCase1.hs): (54, 20)",
                  "testCase1, TestCase1(src/TestCase1.hs): (54, 23)",
                  "a, TestCase1(src/TestCase1.hs): (46, 11)",
                  "a, TestCase1(src/TestCase1.hs): (46, 14)",
                  "b, TestCase1(src/TestCase1.hs): (47, 11)",
                  "b, TestCase1(src/TestCase1.hs): (47, 14)",
                  "a, TestCase1(src/TestCase1.hs): (46, 11)",
                  "a, TestCase1(src/TestCase1.hs): (46, 14)",
                  "recurse, TestCase1(src/TestCase1.hs): (43, 24)",
                  "recurse, TestCase1(src/TestCase1.hs): (43, 27)"
                ]

-- TODO add examples showing how to use the following packages: 
-- control-monad-exception-monadsfd library: Explicitly typed, checked exceptions with stack traces
-- control-monad-exception-monadstf library: Explicitly typed, checked exceptions with stack traces
-- control-monad-exception-mtl library: Explicitly typed, checked exceptions with stack traces
-- control-monad-failure library: A class for monads which can fail with an error.
-- control-monad-failure-mtl library: A class for monads which can fail with an error.
-- control-monad-free library: Free monads and monad transformers
-- control-monad-omega library: A breadth-first list monad.

main = do
  runTestTT $ TestList [
      "monad-failure" ~: monadFailureTests,
      "monad-failure-error-locations" ~: monadFailureWithErrorLocationsTest
    ]
