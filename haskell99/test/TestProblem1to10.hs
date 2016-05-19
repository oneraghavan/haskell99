module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit as Hunit

import Problems1to10
--import qualified Problems11_20
--import qualified Problems21_30

import System.Exit

testProblem1 = TestCase  ( assertEqual
  "should return the actual last item" (Just 4) ( myLast [1,2,3,4]))

testProblem2 = TestCase  ( assertEqual
  "should Nothing" (Nothing) ( myLast ([]::[Int])))

tests = [ testProblem1 , testProblem2 ]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT $ TestList tests
    putStrLn (showCounts cs)
    if (errs > 0 || fails > 0)
        then exitFailure
        else exitSuccess