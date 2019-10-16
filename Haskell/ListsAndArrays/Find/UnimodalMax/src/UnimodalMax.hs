module UnimodalMax (unimodalMax) where

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
                                