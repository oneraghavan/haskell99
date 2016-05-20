module Main where

import TestProblem1to10
import TestProblem10to20
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit as Hunit


main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT $ TestList ( testProblem1to10 ++ testProblems10to20)
    putStrLn (showCounts cs)
    if (errs > 0 || fails > 0)
        then exitFailure
        else exitSuccess