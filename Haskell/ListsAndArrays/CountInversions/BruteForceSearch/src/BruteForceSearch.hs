---------------------------------------------------------
-- Module: BruteForceSearch
-- Counts the number of inversions in a list of items.
-- An inversion is an element x > y s.t. appears before 
-- y in the list (to the "left" of y).
---------------------------------------------------------

module BruteForceSearch (bruteForceSearch) where

-- Input: array A of n distinct integers.
-- Output: the number of inversions of A.
bruteForceSearch :: (Ord a) => [a] -> Integer
bruteForceSearch xs =

    outer xs 0

    -- Go through every element, x, of the list.
    where outer [] rsf = rsf
          outer (x:xs) rsf =
            outer xs (rsf + (inner xs 0))

            -- Check for inversions:
            --  Elements that are smaller than x but that appear
            --   later in xs.
            where inner [] rsf = rsf
                  inner (x':xs) rsf =
                    inner xs (if x > x' then (succ rsf)
                                        else rsf)