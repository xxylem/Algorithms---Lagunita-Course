module FastPower where

-- FastPower
-- Input: positive integers a and b.
-- Output: a^b.
fastPower :: Integer -> Integer -> Integer
fastPower a 1 = a
fastPower a b =
    fastPower (a*a) (b `div` 2) * (if odd b then a else 1)
