import Test.QuickCheck (Positive, getPositive, property)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import FastPower (fastPower)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = 
    describe "Test that fastPower computes powers of positive integers correctly" $ do
        modifyMaxSuccess (const 1000) $ it "Test correctness of fastPower" $ property propComputesPowers


-- FastPower is currently defined only on positive integers
-- TODO consider extending algorithm to all integers.
propComputesPowers :: Positive Integer -> Positive Integer -> Bool
propComputesPowers pa pb =
    fastPower a b == a ^ b
    where a = getPositive pa
          b = getPositive pb