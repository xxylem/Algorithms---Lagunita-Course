import           Data.Matrix                (Matrix)
import qualified Data.Matrix as M
import           Test.Hspec                 (Spec, it, describe)
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import           Test.QuickCheck            (Arbitrary, Gen, Property, arbitrary, choose, forAll, sized, vector, vectorOf)

import           BasicMatrixMult            (basicMatrixMult)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Basic Matrix Multiplication Test" $ do
    it "test identity property" $ prop_identity
    -- describe "Test properties of matrix multiplication using BasicMatrixMult." $ do
    --     modifyMaxSize (const 10) $ it "test identity property" $
    --                                             property propIdentity  
    -- describe "Commutativity property for multiplication." $
    --     do modifyMaxSuccess (const 1000) $ it "propAssociativity" $ property propAssociativity
    -- describe "Distributivity property for multiplication." $
    --     do modifyMaxSuccess (const 1000) $ it "propDistributivity" $ property propDistributivity
    -- describe "BasicMatrixMult provides same result at primitive multiplication." $
        -- do modifyMaxSuccess (const 1) $ it "propGeneralMultiplication" $ 
        --     property propGeneralMultiplication
    
-- propIdentity :: Matrix Integer -> Bool
-- propIdentity x =
--         basicMatrixMult x i ==  x
--     &&  basicMatrixMult i x ==  x
--     where i = M.identity (M.ncols x)


-- instance (Arbitrary a) => (Arbitrary (Matrix a)) where
squareMatrix :: (Arbitrary a) => Gen (Matrix a)
squareMatrix =
    sized $
        \n -> do
            n' <- choose (2, n)
            rows <- vectorOf n' (vector n')
            return $ M.fromLists rows

prop_identity :: Property
prop_identity = forAll squareMatrix $
    \m ->   let i = M.identity (M.ncols m) in    
                basicMatrixMult m i ==  (m :: Matrix Int)
            &&  basicMatrixMult i m ==  m
            

            


-- TODO implement test suite

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

-- propGeneralMultiplication :: MatrixPair
--                             -> Bool
-- propGeneralMultiplication (MatrixPair x y) =
--         basicMatrixMult x y
--     ==  M.multStd x y
