module FastPowerTest where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

import FastPower (fastPower)


main :: IO ()
main = hspec $ do
    describe "Test that fastPower computes powers of positive integers correctly" $ do
        modifyMaxSuccess (const 1000) $ it "Test correctness of fastPower" $ property propComputesPowers


-- FastPower is currently defined only on positive integers
-- TODO consider extending algorithm to all integers.
propComputesPowers :: Positive Integer -> Positive Integer -> Bool
propComputesPowers pa pb =
    fastPower a b == a ^ b
    where a = getPositive pa
          b = getPositive pb