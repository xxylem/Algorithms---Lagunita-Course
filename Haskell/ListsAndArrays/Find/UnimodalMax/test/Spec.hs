import qualified Data.Array as A
import           Test.Hspec        (Spec, describe, it, shouldBe, shouldThrow, anyException)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import           Control.Exception (evaluate)

import           UnimodalMax       (unimodalMax)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    describe "Test suite for finding the max element in a unimodal array." $ do
        it "throws exception if used with empty array" $ do
            evaluate (unimodalMax ua0Elem) `shouldThrow` anyException
        it "the only element in a one element array is the max" $ do
            unimodalMax ua1Elem `shouldBe` 10
        it "test two element arrays" $ do
            unimodalMax ua2Elem1 `shouldBe` 10
            unimodalMax ua2Elem2 `shouldBe` 10
        it "test three element arrays" $ do
            unimodalMax ua3Elem1 `shouldBe` 10
            unimodalMax ua3Elem2 `shouldBe` 10
            unimodalMax ua3Elem3 `shouldBe` 10
            unimodalMax ua3Elem4 `shouldBe` 10
        it "test longer sorted arrays (one direction)" $ do
            unimodalMax uaLongerSorted `shouldBe` 10
            unimodalMax uaLongerReverseSorted `shouldBe` 10
        it "test longer fully unimodal arrays" $ do
            unimodalMax uaLongerMixed1 `shouldBe` 19
            unimodalMax uaLongerMixed2 `shouldBe` 23

 
-- Example unimodal arrays
ua0Elem, ua1Elem, ua2Elem1, ua2Elem2, ua3Elem1,
    ua3Elem2, ua3Elem3, ua3Elem4,
    uaLongerSorted, uaLongerReverseSorted,
    uaLongerMixed1, uaLongerMixed2 :: A.Array Integer Integer
ua0Elem = A.listArray (1, 0) []                             
ua1Elem = A.listArray (1,1) [10]                             
ua2Elem1 = A.listArray (1,2) [9, 10]                             
ua2Elem2 = A.listArray (1,2) [10, 9]                              
ua3Elem1 = A.listArray (1,3) [8, 9, 10]
ua3Elem2 = A.listArray (1,3) [8, 10, 9]
ua3Elem3 = A.listArray (1,3) [9, 10, 8]
ua3Elem4 = A.listArray (1,3) [10, 9, 8]
uaLongerSorted = A.listArray (1,10) [1..10]
uaLongerReverseSorted = A.listArray (1,10) (reverse [1..10])
uaLongerMixed1 = A.listArray (1,10) [1, 3, 4, 5, 19, 17, 15, 14, 12, 2]                   
uaLongerMixed2 = A.listArray (1,10) [1, 17, 23, 22, 21, 18, 15, 13, 9, 5]