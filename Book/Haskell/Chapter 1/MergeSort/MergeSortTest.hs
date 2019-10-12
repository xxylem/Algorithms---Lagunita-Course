module MergeSortTest where

import MergeSort (mergeSort)
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec $ do
    describe "MergeSort sorts lists of integers." $
        do modifyMaxSuccess (const 1000) $ it "propSortsIntegerLists" $ property propSortsIntegerLists
    describe "MergeSort sorts characters in strings." $
        do modifyMaxSuccess (const 1000) $ it "propSortsStrings" $ property propSortsStrings




propSortsIntegerLists :: [Integer] -> Bool
propSortsIntegerLists xs =
    mergeSort xs == sort xs

propSortsStrings :: String -> Bool
propSortsStrings xs =
    mergeSort xs == sort xs

