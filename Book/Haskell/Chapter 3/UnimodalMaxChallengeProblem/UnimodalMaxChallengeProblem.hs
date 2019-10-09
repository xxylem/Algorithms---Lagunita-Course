module UnimodalMaxChallengeProblem where

import Data.Array

-- Find Max Element of Unimodal Array
-- 
-- INPUT: A unimodal array of n distinct elements, uniArr.
-- OUTPUT: The maximum element in uniArr.
unimodalMax :: (Integral i, Ix i, Ord e) => Array i e -> e
unimodalMax uniArr =
    go (bounds uniArr)
    where   go (il, iu)
                | il == iu = uniArr ! il
                | otherwise = let midPoint1 = (il + iu) `div` 2 
                                  midPoint2 = midPoint1 + 1 in
                                if uniArr ! midPoint1 < uniArr ! midPoint2
                                then go (midPoint2, iu)
                                else go (il, midPoint1)

-- Example unimodal arrays
uArr1 :: Array Integer Integer                                
uArr1 = listArray (1,1) [10]
uArr2 :: Array Integer Integer                                
uArr2 = listArray (1,2) [9, 10]
uArr3 :: Array Integer Integer                                
uArr3 = listArray (1,2) [10, 9]
uArr4 :: Array Integer Integer                                
uArr4 = listArray (1,3) [8, 9, 10]
uArr5 :: Array Integer Integer                                
uArr5 = listArray (1,3) [8, 10, 9]
uArr6 :: Array Integer Integer                                
uArr6 = listArray (1,3) [9, 10, 8]
uArr7 :: Array Integer Integer                                
uArr7 = listArray (1,3) [10, 9, 8]
uArr8 :: Array Integer Integer                                
uArr8 = listArray (1,10) [1..10]
uArr9 :: Array Integer Integer                                
uArr9 = listArray (1,10) (reverse [1..10])
uArr10 :: Array Integer Integer                                
uArr10 = listArray (1,10) [1, 3, 4, 5, 19, 17, 15, 14, 12, 2]
uArr11 :: Array Integer Integer                                
uArr11 = listArray (1,10) [1, 17, 23, 22, 21, 18, 15, 13, 9, 5]

