module BasicMatrixMultTest where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)

import qualified Data.Matrix as M

import BasicMatrixMult (basicMatrixMult)

-- TODO make generator for matrices.

main :: IO ()
main = hspec $ do
    describe "Identity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propIdentity" $ property propIdentity
    describe "Commutativity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propAssociativity" $ property propAssociativity
    describe "Distributivity property for multiplication." $
        do modifyMaxSize (const 1000) $ it "propDistributivity" $ property propDistributivity
    describe "BasicMatrixMult provides same result at primitive multiplication." $
        do modifyMaxSize (const 1000) $ it "propGeneralMultiplication" $ property propGeneralMultiplication
    
propIdentity :: M.Matrix Integer -> Bool
propIdentity x =
        basicMatrixMult x i ==  x
    &&  basicMatrixMult i x ==  x
    where i = M.identity (M.ncols x)


propAssociativity :: M.Matrix Integer 
                    -> M.Matrix Integer 
                    -> M.Matrix Integer 
                    -> Bool
propAssociativity x y z =
        basicMatrixMult x (basicMatrixMult y z) 
    ==  basicMatrixMult (basicMatrixMult x y) z


propDistributivity :: M.Matrix Integer 
                    -> M.Matrix Integer 
                    -> M.Matrix Integer 
                    -> Bool
propDistributivity x y z =
        basicMatrixMult x (y + z)
    ==  basicMatrixMult x y + basicMatrixMult x z

propGeneralMultiplication :: M.Matrix Integer 
                            -> M.Matrix Integer 
                            -> Bool
propGeneralMultiplication x y =
        basicMatrixMult x y
    ==  M.multStd x y
