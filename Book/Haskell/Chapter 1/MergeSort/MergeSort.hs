module MergeSort (mergeSort) where

-- Input: array A of n distinct integers. (We make this more general.)
-- Output: array with the same integers, sorted from
-- smallest to largest.
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    merge as' bs'
        where (as, bs) = splitAt (length xs `div` 2) xs
              as' = mergeSort as
              bs' = mergeSort bs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = 
    if x <= y then x : merge xs        (y:ys)
              else y : merge (x:xs)    ys

checkListIsOrdered :: (Ord a) => [a] -> Bool
checkListIsOrdered [] = True
checkListIsOrdered [x] = True
checkListIsOrdered (x1:x2:xs) = (x1 <= x2) && checkListIsOrdered (x2:xs)



