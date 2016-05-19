module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit as Hunit

import Problems1to10
--import qualified Problems11_20
--import qualified Problems21_30

import System.Exit

--testProblem1 = TestCase  ( assertEqual
--  "should return the actual last item" (Just 4) ( myLast [1,2,3,4]))
--
--testProblem2 = TestCase  ( assertEqual "should return Nothing when given empty list" (Nothing) ( myLast ([]::[Int])))

shouldTestMylast =
        [
          TestCase  ( assertEqual "should return the actual last item" (Just 4) ( myLast [1,2,3,4])),
          TestCase  ( assertEqual "should return Nothing when given empty list" (Nothing) ( myLast ([]::[Int])))
         ]

shouldTestMyButlast =
        [
          TestCase  ( assertEqual "should return the actual but one item" (Just 3) ( myButLast [1,2,3,4])),
          TestCase  ( assertEqual "should return Nothing when given empty list" (Nothing) ( myButLast ([]::[Int])))
         ]

shouldTestElementAt =
        [
          TestCase  ( assertEqual "should return the actual element at" 2 ( elementAt [1,2,7,1] 2)),
          TestCase  ( assertEqual "should return the actual element at" 9 ( elementAt [1,7,1,9] 4))
         ]

shouldTestmyLength =
        [
          TestCase  ( assertEqual "should return the actual lenght of the array" 4 ( myLength [1,2,7,1])),
          TestCase  ( assertEqual "should return 0 for empty array" 0 ( myLength []))
         ]

shouldTestmyReverse =
        [
          TestCase  ( assertEqual "should reverse the  array" ([1,7,2,1]) ( myReverse [1,2,7,1])),
          TestCase  ( assertEqual "should return revesre of array" ("edcba") ( myReverse "abcde")),
          TestCase  ( assertEqual "should return empty array for empty array" ([]) ( myReverse ([]::[Int])))
         ]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT $ TestList ( shouldTestMylast ++ shouldTestMyButlast ++ shouldTestElementAt ++ shouldTestmyLength ++ shouldTestmyReverse)
    putStrLn (showCounts cs)
    if (errs > 0 || fails > 0)
        then exitFailure
        else exitSuccess