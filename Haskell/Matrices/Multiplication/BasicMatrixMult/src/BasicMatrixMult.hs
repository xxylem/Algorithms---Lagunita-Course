module BasicMatrixMult where

import           Data.Matrix (Matrix)
import qualified Data.Matrix as M
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- Input: n x n integer matrices X and Y.
-- Output: Z = X · Y.
basicMatrixMult :: (Num a) => Matrix a -> Matrix a -> Matrix a
basicMatrixMult x y =
    
    -- Start at row 1 col 1 (Data.Matrix uses 1-indexing)
    go 1 1 z
        where   n = M.nrows x -- Assumes both matrices are n x n
                z = M.zero n n -- Init the Z matrix to all zeroes
                go i j z 
                    -- Reached the last entry of the last row and column
                    -- Returns the completed Z matrix
                    | i == n && j == n = M.setElem (dot (M.getRow i x)
                                                    (M.getCol j y))
                                                (i, j)
                                                z
                    -- Reached the end of a row
                    -- Carries on from the next column
                    | i == n = go 1 (j+1) (M.setElem (dot (M.getRow i x)
                                                        (M.getCol j y))
                                                    (i, j)
                                                    z)
                    -- Moves to the next row, same column
                    | otherwise = go (i+1) j (M.setElem (dot (M.getRow i x)
                                                            (M.getCol j y))
                                                (i, j) 
                                                z)

-- Dot product of two vectors x and y
dot :: (Num a) => Vector a -> Vector a -> a
dot x y =
    go 0 0 -- Starts from entry 0 of x and y, with dot product result init to 0
            -- (Data.Vector uses 0-indexing)
        where n = V.length x
              go i rsf 
                | i >= n = rsf
                | otherwise = go (i+1) (rsf
                                        -- Add the product of the ith entry in x and y
                                        +   (   x V.! i 
                                            *   y V.! i)) 

-- Example matrices X and Y
x :: Matrix Integer
x = M.fromLists [ [1, 2, 3]
                , [5, 6, 7]
                , [7, 8, 9]]
y :: Matrix Integer
y = M.fromLists [ [2, 5, 3]
                , [7, 6, 1]
                , [4, 9, 0]]

z :: Matrix Integer
z = basicMatrixMult x y