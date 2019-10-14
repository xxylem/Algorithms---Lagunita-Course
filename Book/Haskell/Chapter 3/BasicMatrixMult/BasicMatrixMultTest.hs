module BasicMatrixMultTest where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxSize)

import qualified Data.Matrix as M

import BasicMatrixMult (basicMatrixMult)

-- TODO make generator for matrices.

data MatrixPair = MatrixPair { matX :: M.Matrix Integer
                             , matY :: M.Matrix Integer
                             }
                             deriving (Eq, Show)

instance Arbitrary MatrixPair where
    arbitrary = 
        sized $ \n -> do
            valsX <- vectorOf (n * n) arbitrary
            valsY <- vectorOf (n * n) arbitrary
            return $ MatrixPair (M.fromList n n valsX)
                                (M.fromList n n valsY)  

-- instance (Arbitrary a) => Int -> (Arbitrary (M.Matrix a)) where
--     arbitrary n = do
--         vals <- vector (n * n)
--         return $ M.fromList n n vals

instance (Arbitrary a) => (Arbitrary (M.Matrix a)) where
    arbitrary = 
        sized $ \n -> do
            vals <- vectorOf (n * n) arbitrary
            return (M.fromList n n vals)

main :: IO ()
main = hspec $ do
    describe "Test properties of matrix multiplication using BasicMatrixMult." $ do
        modifyMaxSize (const 10) $ it "test identity property" $
                                                property propIdentity
        
        
    -- describe "Commutativity property for multiplication." $
    --     do modifyMaxSuccess (const 1000) $ it "propAssociativity" $ property propAssociativity
    -- describe "Distributivity property for multiplication." $
    --     do modifyMaxSuccess (const 1000) $ it "propDistributivity" $ property propDistributivity
    -- describe "BasicMatrixMult provides same result at primitive multiplication." $
        -- do modifyMaxSuccess (const 1) $ it "propGeneralMultiplication" $ 
        --     property propGeneralMultiplication
    
propIdentity :: M.Matrix Integer -> Bool
propIdentity x =
        basicMatrixMult x i ==  x
    &&  basicMatrixMult i x ==  x
    where i = M.identity (M.ncols x)


-- propAssociativity :: M.Matrix Integer 
--                     -> M.Matrix Integer 
--                     -> M.Matrix Integer 
--                     -> Bool
-- propAssociativity x y z =
--         basicMatrixMult x (basicMatrixMult y z) 
--     ==  basicMatrixMult (basicMatrixMult x y) z


-- propDistributivity :: M.Matrix Integer 
--                     -> M.Matrix Integer 
--                     -> M.Matrix Integer 
--                     -> Bool
-- propDistributivity x y z =
--         basicMatrixMult x (y + z)
--     ==  basicMatrixMult x y + basicMatrixMult x z

propGeneralMultiplication :: MatrixPair
                            -> Bool
propGeneralMultiplication (MatrixPair x y) =
        basicMatrixMult x y
    ==  M.multStd x y
