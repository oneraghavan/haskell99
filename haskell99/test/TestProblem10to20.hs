module TestProblem10to20 where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit as Hunit

import Problems10to20

import System.Exit

shouldTestEncodeModified =
        [
          TestCase  ( assertEqual "should pack the array" [Multiple 6 'a',Single 'b',Multiple 2 'c', Single 'd',Multiple 4 'e'] (encodeModified "aaaabccaadeeee" ))
         ]

testProblems10to20 = shouldTestEncodeModified
