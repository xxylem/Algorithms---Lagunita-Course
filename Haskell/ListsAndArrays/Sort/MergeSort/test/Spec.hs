import Data.List                  (sort)
import Test.Hspec                 (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck            (property)

import MergeSort                  (mergeSort)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
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

