import Test.QuickCheck            (Gen, Property, choose, frequency, forAll)
import Test.Hspec                 (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)

import RecIntMult                 (recIntMult)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Test suite for recIntMult multiplication" $ do

    it "test identity property" $ prop_identity

    it "test commutativity" $ prop_commutativity

    -- it "test associativity" $ prop_associativity

    modifyMaxSuccess (const 10000) $ it "test distributivity" $ prop_distributivity

    it "test multiplies correctly" $ prop_multiplies_correctly

 
twoIntegersMinMax :: Int -> Int -> Gen (Integer, Integer)
twoIntegersMinMax minSize maxSize = do
    x <- choose (minSize, maxSize)
    y <- choose (minSize, maxSize)
    return (fromIntegral x, fromIntegral y)

threeIntegersMinMax :: Int -> Int -> Gen (Integer, Integer, Integer)
threeIntegersMinMax minSize maxSize = do
    x <- choose (minSize, maxSize)
    y <- choose (minSize, maxSize)
    z <- choose (minSize, maxSize)
    return (fromIntegral x, fromIntegral y, fromIntegral z)
        
twoIntegersWithNumDigitsAPowerOf2 :: Gen (Integer, Integer)
twoIntegersWithNumDigitsAPowerOf2 =
    frequency   [ (5, twoIntegersMinMax 0 99)
                , (4, twoIntegersMinMax 1000 9999)
                , (3, twoIntegersMinMax 10000000 99999999)
                , (2, twoIntegersMinMax 1000000000000000 9999999999999999) ]

threeIntegersWithNumDigitsAPowerOf2 :: Gen (Integer, Integer, Integer)
threeIntegersWithNumDigitsAPowerOf2 =
    frequency   [ (5, threeIntegersMinMax 0 99)
                , (4, threeIntegersMinMax 1000 9999)
                , (3, threeIntegersMinMax 10000000 99999999)
                , (2, threeIntegersMinMax 1000000000000000 9999999999999999) ]
 
-- Technically recIntMult isn't defined on integers of different numbers of digits,
--  but the test works regardless.                
prop_identity :: Property
prop_identity = forAll twoIntegersWithNumDigitsAPowerOf2 $
    \(x, y) -> recIntMult x 1 == x && recIntMult y 1 == y             

prop_commutativity :: Property
prop_commutativity = forAll twoIntegersWithNumDigitsAPowerOf2 $
    \(x, y) -> recIntMult x y == recIntMult y x

-- TODO Does not hold due to producing integers with numbers of digits
-- that are not powers of 2 in the intermediate steps.
-- prop_associativity :: Property
-- prop_associativity = forAll threeIntegersWithNumDigitsAPowerOf2 $
--     \(x, y, z) ->       recIntMult x (recIntMult y z) 
--                     ==  recIntMult (recIntMult x y) z

-- TODO Passes but may have same issue as prop_associativity
-- in that y + z may produce an invalid number.
prop_distributivity :: Property
prop_distributivity = forAll threeIntegersWithNumDigitsAPowerOf2 $
    \(x, y, z) ->       recIntMult x (y + z)
                    ==  recIntMult x y + recIntMult x z

-- Check correctness against primitive multiplication.
prop_multiplies_correctly :: Property
prop_multiplies_correctly = forAll twoIntegersWithNumDigitsAPowerOf2 $
    \(x, y) -> recIntMult x y == x * y
