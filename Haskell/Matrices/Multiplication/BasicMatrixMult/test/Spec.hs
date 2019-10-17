import           Data.Matrix                (Matrix)
import qualified Data.Matrix as M
import           Test.Hspec                 (Spec, it, describe)
import           Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import           Test.QuickCheck            (Arbitrary, Gen, Property, choose, forAll, sized, vector, vectorOf)

import           BasicMatrixMult            (basicMatrixMult)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Basic Matrix Multiplication Test" $ do

    it "test identity property" $ prop_identity

    it "test correct multiplication of square matrices" $ prop_correctly_multiplies_arbitrary_square_matrices

    it "test associativity property" $ prop_associativity

    it "test distributivity property" $ prop_distributivity


squareMatrix :: (Arbitrary a) => Gen (Matrix a)
squareMatrix =
    sized $
        \n -> do
            let n' = max 1 n
            n'' <- choose (1, n')
            rows <- vectorOf n'' (vector n'')
            return $ M.fromLists rows

prop_identity :: Property
prop_identity = forAll squareMatrix $
    \m ->   let i = M.identity (M.ncols m) in    
                basicMatrixMult m i ==  (m :: Matrix Int)
            &&  basicMatrixMult i m ==  m
            
twoSquareMatricesSameSize :: (Arbitrary a) => Gen (Matrix a, Matrix a)
twoSquareMatricesSameSize =
    sized $
        \n -> do
            let n' = max 1 n
            n'' <- choose (1, n')
            rows1 <- vectorOf n'' (vector n'')
            rows2 <- vectorOf n'' (vector n'')
            return (M.fromLists rows1, M.fromLists rows2)

prop_correctly_multiplies_arbitrary_square_matrices :: Property
prop_correctly_multiplies_arbitrary_square_matrices =
    forAll twoSquareMatricesSameSize $
    \(x, y) -> basicMatrixMult x y == M.multStd x (y :: Matrix Int)
          
threeSquareMatricesSameSize :: (Arbitrary a) => Gen (Matrix a, Matrix a, Matrix a)
threeSquareMatricesSameSize =
    sized $
        \n -> do
            let n' = max 1 n
            n'' <- choose (1, n')
            rows1 <- vectorOf n'' (vector n'')
            rows2 <- vectorOf n'' (vector n'')
            rows3 <- vectorOf n'' (vector n'')
            return (M.fromLists rows1, M.fromLists rows2, M.fromLists rows3)


prop_associativity :: Property
prop_associativity =
    forAll threeSquareMatricesSameSize $
    \(x, y, z) -> basicMatrixMult x (basicMatrixMult y z) == basicMatrixMult (basicMatrixMult x y) (z :: Matrix Int)

prop_distributivity :: Property
prop_distributivity =
    forAll threeSquareMatricesSameSize $
    \(x, y, z) -> basicMatrixMult x (y + z) == basicMatrixMult x y + basicMatrixMult x (z :: Matrix Int)
