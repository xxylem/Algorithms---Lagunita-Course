module KaratsubaTest where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Control.Monad (guard)

import Karatsuba (karatsuba)

-- TODO modify tests s.t. only numbers of the same length and whose
-- length is a power of two are generated for the tests.
--    OR: upgrade Karatsuba function to operate on all integers.


main :: IO ()
main = hspec $ do
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
    
propIdentity :: Integer -> Bool
propIdentity x =
        karatsuba x 1
    ==  x
    &&  karatsuba 1 x
    ==  x

propCommutativity :: Integer -> Integer -> Bool
propCommutativity x y = 
        karatsuba x y 
    ==  karatsuba y x

propAssociativity :: Integer -> Integer -> Integer -> Bool
propAssociativity x y z =
        karatsuba x (karatsuba y z) 
    ==  karatsuba (karatsuba x y) z


propDistributivity :: Integer -> Integer -> Integer -> Bool
propDistributivity x y z =
        karatsuba x (y + z)
    ==  karatsuba x y + karatsuba x z

propGeneralMultiplication :: Integer -> Integer -> Bool
propGeneralMultiplication x y =
        karatsuba x y
    ==  x * y
