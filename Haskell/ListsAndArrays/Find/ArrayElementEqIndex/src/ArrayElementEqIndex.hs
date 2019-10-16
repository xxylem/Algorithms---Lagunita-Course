module ArrayElementEqIndex (arrayElementEqIndex) where

import Data.Array (Array, (!), bounds)

-- Given a sorted array (smallest to largest) of n distinct integers
-- (positive, negative, or zero), return True if there exists an
--  index i such that the element at that index is equal to the index.

-- INPUT: A sorted array of n distinct integers, arr.
-- OUTPUT: True if there is i s.t. arr ! i == i. False otherwise.
-- ASSUMES: 1-indexing in the array.
arrayElementEqIndex :: Array Integer Integer -> Bool
arrayElementEqIndex arr =
    -- Start the search with the full range of the array.
    go (bounds arr)
    where  
        go (il, iu)
                -- Base case (n=1): Check if the one element meets the condition.
                | il == iu = arr ! il == il
                -- Base case (n=2): Check if either element meets the condition.
                | abs (il - iu) < 2 = 
                    arr ! il == il || arr ! iu == iu
                -- Inductive step:
                | otherwise = 
                    -- Choose a point in the middle of the search window.
                    let midPoint = (il + iu) `div` 2 in
                    -- Compare A[i] to i:
                    case compare (arr ! midPoint) midPoint of
                        -- If they equal, the condition is met.
                        EQ -> True
                        -- If A[i] > i, then the condition cannot be met,
                        --  since the integers are sorted and distinct.
                        GT -> False
                        -- If A[i] < i, the condition could possibly be met in
                        -- the right half of the search window.
                        LT -> go (midPoint, iu)
