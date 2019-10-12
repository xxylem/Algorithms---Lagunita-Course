module RecIntMultTest where

    import Test.QuickCheck
    import Test.Hspec
    import Test.Hspec.Core.QuickCheck (modifyMaxSize)
    import Control.Monad (guard)
    
    import RecIntMult (recIntMult)
    
    -- TODO modify tests s.t. only numbers of the same length and whose
    -- length is a power of two are generated for the tests.
    --    OR: upgrade RecIntMult function to operate on all integers.
    
    
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
        describe "RecIntMult provides same result at primitive multiplication." $
            do modifyMaxSize (const 1000) $ it "propGeneralMultiplication" $ property propGeneralMultiplication
        
    propIdentity :: Integer -> Bool
    propIdentity x =
            recIntMult x 1
        ==  x
        &&  recIntMult 1 x
        ==  x
    
    propCommutativity :: Integer -> Integer -> Bool
    propCommutativity x y = 
            recIntMult x y 
        ==  recIntMult y x
    
    propAssociativity :: Integer -> Integer -> Integer -> Bool
    propAssociativity x y z =
            recIntMult x (recIntMult y z) 
        ==  recIntMult (recIntMult x y) z
    
    
    propDistributivity :: Integer -> Integer -> Integer -> Bool
    propDistributivity x y z =
            recIntMult x (y + z)
        ==  recIntMult x y + recIntMult x z
    
    propGeneralMultiplication :: Integer -> Integer -> Bool
    propGeneralMultiplication x y =
            recIntMult x y
        ==  x * y
    