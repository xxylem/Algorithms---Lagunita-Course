module RecIntMult (recIntMult) where

-- Input: two n-digit positive integers x and y.
-- Output: the product x · y.
-- Assumption: n is a power of 2.
recIntMult :: Integer -> Integer -> Integer
recIntMult x y =

    go x y (size x)

    where 
          -- Get the number of digits in x (assumes y is same)
          size x = length $ show x

          -- Base case: The product of two one digit numbers uses primitive mult.
          go x y 1 = x * y

          -- Otherwise: 
          go x y n = 

            -- Add up the partial products
                (10 ^ n) * ac
            +   (10 ^ nOver2) * (ad + bc)
            +   bd

            where nOver2 = n `div` 2
                  tenToNover2 = 10 ^ nOver2

                  -- Split x and y in half
                  a = x `div` tenToNover2
                  b = x `mod` tenToNover2
                  c = y `div` tenToNover2
                  d = y `mod` tenToNover2

                  -- Recursively compute partial products
                  ac = go a c nOver2
                  ad = go a d nOver2
                  bc = go b c nOver2
                  bd = go b d nOver2
            