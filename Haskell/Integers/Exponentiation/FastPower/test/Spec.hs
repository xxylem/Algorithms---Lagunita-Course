import Test.QuickCheck            (Positive, getPositive, property)
import Test.Hspec                 (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)

import FastPower                  (fastPower)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = 
    describe "Test that fastPower computes powers of positive integers correctly" $ do
        modifyMaxSuccess (const 1000) $ it "Test correctness of fastPower" $ property prop_computes_powers_correctly


-- FastPower is currently defined only on positive integers
prop_computes_powers_correctly :: Positive Integer -> Positive Integer -> Bool
prop_computes_powers_correctly pa pb =
    fastPower a b == a ^ b
    where a = getPositive pa
          b = getPositive pb