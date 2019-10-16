import           Data.Matrix                (Matrix)
import qualified Data.Matrix as M
import           Test.Hspec                 (Spec)
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import           Test.QuickCheck            (arbitrary)

import           BasicMatrixMult            (basicMatrixMult)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = undefined
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
    
propIdentity :: Matrix Integer -> Bool
propIdentity x =
        basicMatrixMult x i ==  x
    &&  basicMatrixMult i x ==  x
    where i = M.identity (M.ncols x)

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
