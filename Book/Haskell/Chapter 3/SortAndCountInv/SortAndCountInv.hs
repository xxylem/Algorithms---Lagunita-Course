module SortAndCountInv where

main :: IO ()
main = do
    f <- readFile "IntegerArray.txt"
    let ls = lines f
        ints = map read ls :: [Integer]
        (_, numInv) = sortAndCountInv ints
    print numInv
    return ()


-- 
-- Input: array A of n distinct integers.
-- Output: sorted array B with the same integers, and
-- the number of inversions of A.
sortAndCountInv :: (Ord a) => [a] -> ([a], Int)
sortAndCountInv [] = ([], 0)
sortAndCountInv [x] = ([x], 0)
sortAndCountInv xs =

    -- Return the final merged list and sum of all inversions
    (merged, leftInv + rightInv + splitInv)

    where   -- Split the list roughly in half
            (lefts, rights) = splitAt (length xs `div` 2) xs

            -- Sort the half lists and count the number of inversions in each half
            (lefts', leftInv) = sortAndCountInv lefts
            (rights', rightInv) = sortAndCountInv rights

            -- Merge the lists back together and count the inversions split between
            -- the two halves
            (merged, splitInv) = mergeAndCountSplitInv lefts' rights'


-- Input: sorted arrays C and D (length n/2 each).
-- Output: sorted array B (length n) and the number of
-- split inversions.
-- Simplifying assumption: n is even.
mergeAndCountSplitInv :: (Ord a) => [a] -> [a] -> ([a], Int)
mergeAndCountSplitInv xs ys =
    go xs ys 0
    
    where go xs [] invs = (xs, invs)
          go [] ys invs = (ys, invs)
          go (x:xs) (y:ys) invs =

            -- The element in the second list is smaller and so is
            -- a split inversion with all elements left in the first list.
            if x > y then 
                let (merged, invs') = go (x:xs) ys (invs + length (x:xs)) in
                (y:merged, invs')
            
                -- The element in the first list is smaller and so is not
                -- an inversion.
                else
                let (merged, invs') = go xs (y:ys) invs in
                (x:merged, invs')
            