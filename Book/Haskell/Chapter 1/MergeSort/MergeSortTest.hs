module MergeSortTest where

import MergeSort
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec $ do
    describe "MergeSort sorts lists of integers." $
        do modifyMaxSuccess (const 1000) $ it "propSortsLists" $ property propSortsLists



propSortsLists :: [Integer] -> Bool
propSortsLists xs =
    mergeSort xs == sort xs