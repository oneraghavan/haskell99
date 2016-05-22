module TestProblem10to20 where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit as Hunit

import Problems10to20

import System.Exit

shouldTestEncodeModified =
        [
          TestCase  ( assertEqual "should encode the array" [Multiple 6 'a',Single 'b',Multiple 2 'c', Single 'd',Multiple 4 'e'] (encodeModified "aaaabccaadeeee" ))
         ]

shouldTestDecodeModified =
        [
          TestCase  ( assertEqual "should decode the array" "aaaaaabccdeeee" (decodeModified [Multiple 6 'a',Single 'b',Multiple 2 'c', Single 'd',Multiple 4 'e'] )),
          TestCase  ( assertEqual "should decode the empty array" [] (decodeModified ([]::[Encoded Int]) ))
         ]

shouldTestDupli =
        [
          TestCase  ( assertEqual "should duplicate the array" [1,1,2,2,3,3] (dupli [1,2,3] )),
          TestCase  ( assertEqual "should return empty when given array" [] (dupli ([]::[Int]) ))
         ]

shouldTestRepli =
        [
          TestCase  ( assertEqual "should pack the array" "aaabbbccc" (repli "abc" 3))
         ]

shouldTestdropEvery =
        [
          TestCase  ( assertEqual "should pack the array" "abdeghj" (dropEvery "abcdefghij" 3)),
          TestCase  ( assertEqual "should pack the array" [] (dropEvery ([]::[Int]) 3))
         ]

testProblems10to20 = shouldTestEncodeModified ++ shouldTestDecodeModified ++ shouldTestDupli ++
                      shouldTestRepli ++ shouldTestdropEvery
