module ArrayElementEqIndexChallengeProblem where

import Data.Array

-- Given a sorted array (smallest to largest) of n distinct integers
-- (positive, negative, or zero), return True if there exists an
--  index i such that the element at that index is equal to the index.
-- 
-- INPUT: A sorted array of n distinct integers, arr.
-- OUTPUT: True if there is i s.t. arr ! i == i. False otherwise.
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

-- Example sorted arrays
arr1 :: Array Integer Integer                                
arr1 = listArray (1,1) [10]
arr2 :: Array Integer Integer                                
arr2 = listArray (1,2) [9, 10]
arr3 :: Array Integer Integer                                
arr3 = listArray (1,1) [1]
arr4 :: Array Integer Integer                                
arr4 = listArray (1,3) [-1, 0, 3]
arr5 :: Array Integer Integer                                
arr5 = listArray (1,3) [-1, 0, 1]
arr6 :: Array Integer Integer                                
arr6 = listArray (1,3) [0, 2, 3]
arr7 :: Array Integer Integer                                
arr7 = listArray (1,10) [2..11]
arr8 :: Array Integer Integer                                
arr8 = listArray (1,10) [1..10]
arr9 :: Array Integer Integer                                
arr9 = listArray (1,10) [(-10)..(-1)]
arr10 :: Array Integer Integer                                
arr10 = listArray (1,10) [1, 3, 4, 5, 19, 56, 2345, 253232, 523232, 35231232]
arr11 :: Array Integer Integer                                
arr11 = listArray (1,10) $ [(-10)..(-2)]++[10]

