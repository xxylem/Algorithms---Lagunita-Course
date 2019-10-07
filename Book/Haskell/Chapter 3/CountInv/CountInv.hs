module CountInv where

-- Counts the number of inversions in the given list.
-- Input: array A of n distinct integers.
-- Output: the number of inversions of A.
countInv :: (Ord a) => [a] -> Integer
countInv [] = 0
countInv (_:[]) = 0
countInv xs =

        leftInv 
    +   rightInv 
    +   splitInv

    where   (lefts, rights) = splitAt ((length xs) `div` 2) xs
            leftInv = countInv lefts
            rightInv = countInv rights
            splitInv = countSplitInv xs

-- Left undefined per book description.
countSplitInv :: (Ord a) => [a] -> Integer
countSplitInv = undefined


