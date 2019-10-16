module Main where

import SortAndCountInv (sortAndCountInv)

main :: IO ()
main = assignmentOne

assignmentOne :: IO ()
assignmentOne = do
    f <- readFile "IntegerArray.txt"
    let ls = lines f
        ints = map read ls :: [Integer]
        (_, numInv) = sortAndCountInv ints
    print numInv
    return ()