import           Data.List         (sort)
import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import           Test.QuickCheck   (SortedList, getSorted, property)
import           Data.Set          (Set)
import qualified Data.Set as S

import BruteForceSearch (bruteForceSearch)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    describe "Testing BruteForceSearch: Counts inversions in lists." $ do
        it "an empty list has no inversions" $ do
            bruteForceSearch ([] :: [Integer]) `shouldBe` 0

        it "sorted lists have no inversions" $ 
            property propSortedListsHaveZeroInversions

        it "reverse sorted lists have max (n-1)(n)/2 inversions" $
            property propReverseSortedListsHaveMaxInversions

        it "one element lists have zero inversions" $
            property propOneElementListsHaveZeroInversions

        it "test example lists from two to five elements" $ do
            bruteForceSearch twoElem0inv `shouldBe` 0
            bruteForceSearch twoElem1inv `shouldBe` 1
            bruteForceSearch threeElem0inv `shouldBe` 0
            bruteForceSearch threeElem1inv `shouldBe` 1
            bruteForceSearch threeElem2inv `shouldBe` 2
            bruteForceSearch threeElem3inv `shouldBe` 3

-- Examples for Hspec
twoElem0inv, twoElem1inv, threeElem0inv, 
    threeElem1inv, threeElem2inv, threeElem3inv :: [Integer]
twoElem0inv = [4, 7]
twoElem1inv = [9, -5]
threeElem0inv = [-50, 0, 5000]
threeElem1inv = [4, 6, 5]
threeElem2inv = [6, 4, 5]
threeElem3inv = [6, 5, 4]

-- Properties for QuickCheck
propOneElementListsHaveZeroInversions :: Integer -> Bool
propOneElementListsHaveZeroInversions x =
    bruteForceSearch [x] == 0

propSortedListsHaveZeroInversions :: SortedList Integer -> Bool
propSortedListsHaveZeroInversions xs =
    bruteForceSearch (getSorted xs) == 0

-- Using Set to generate the Integers guarantees there are no distinct elements.
propReverseSortedListsHaveMaxInversions :: Set Integer -> Bool
propReverseSortedListsHaveMaxInversions intSet =
    bruteForceSearch xs == toInteger (((n - 1) * n) `div` 2)
    where   xs = S.toDescList intSet
            n = length xs

