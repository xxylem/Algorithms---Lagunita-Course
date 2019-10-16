import Test.QuickCheck (Positive(..), property)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Karatsuba (karatsuba)

-- TODO modify tests s.t. only numbers of the same length and whose
-- length is a power of two are generated for the tests.
--    OR: upgrade Karatsuba function to operate on all integers.

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    describe "Identity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propIdentity" $ property propIdentity
    describe "Commutativity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propCommutativity" $ property propCommutativity
    describe "Associativity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propAssociativity" $ property propAssociativity
    describe "Distributivity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propDistributivity" $ property propDistributivity
    describe "karatsuba provides same result at primitive multiplication." $
        do modifyMaxSize (const 1000) $ it "propGeneralMultiplication" $ property propGeneralMultiplication
    
propIdentity :: Positive Integer -> Bool
propIdentity (Positive x) =
        karatsuba x 1
    ==  x
    &&  karatsuba 1 x
    ==  x

propCommutativity :: Positive Integer 
                  -> Positive Integer 
                  -> Bool
propCommutativity (Positive x) (Positive y) = 
        karatsuba x y 
    ==  karatsuba y x

propAssociativity :: Positive Integer 
                  -> Positive Integer 
                  -> Positive Integer 
                  -> Bool
propAssociativity (Positive x) (Positive y) (Positive z) =
        karatsuba x (karatsuba y z) 
    ==  karatsuba (karatsuba x y) z


propDistributivity :: Positive Integer 
                   -> Positive Integer 
                   -> Positive Integer 
                   -> Bool
propDistributivity (Positive x) (Positive y) (Positive z) =
        karatsuba x (y + z)
    ==  karatsuba x y + karatsuba x z

propGeneralMultiplication :: Positive Integer 
                          -> Positive Integer 
                          -> Bool
propGeneralMultiplication (Positive x) (Positive y) =
        karatsuba x y
    ==  x * y
