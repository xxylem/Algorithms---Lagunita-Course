module ClosestPair (sortByY, sortByX, Point(..), closestPair) where

import Data.Sort as S

-- A point in  the two dimensional XY plane.
data Point =
    Point { x :: Int
          , y :: Int }
          deriving (Eq, Ord, Show)

-- ClosestPair (Preliminary Version)
-- Input: two copies Px and Py of n >= 2 points in the
-- plane, sorted by x- and y-coordinate, respectively.
-- Output: the pair pi, pj of distinct points with smallest
-- Euclidean distance between them.
closestPair :: [Point] -> [Point] -> (Point, Point)
closestPair [] _ = undefined        -- Not defined on n < 2
closestPair [_] _ = undefined       -- Not defined on n < 2
closestPair [p1, p2] _ = (p1, p2)   -- Base case 1: n=2
closestPair [p1, p2, p3] _ =        -- Base case 2: n=3
    best
    where d_12 = dist (p1, p2)
          d_13 = dist (p1, p3)
          d_23 = dist (p2, p3)
          -- Manually compute the distances between the three points and return
          -- the closest pair among them.
          (_, best) = minimum [(d_12, (p1, p2)), (d_13, (p1, p3)), (d_23, (p2, p3))]

closestPair px py =                 -- Inductive step: n>=4
        case ss of
            -- If there is a closest split pair, find the min amongst
            -- the best left pair, split pair and right pair.
            Just (d_ss, s1, s2) -> 
                let (_, best) = minimum [(d_ls, ls), (d_rs, rs), (d_ss, (s1, s2))] in
                        best
            -- If there is no closest split pair, find the min amongst
            -- only the best left pair and best right pair.
            Nothing ->
                let (_, best) = minimum [(d_ls, ls), (d_rs, rs)] in
                        best

        where   n = length px
                nD2 = n `div` 2   

                -- Since Px is already sorted by X-coordinate,
                -- Lx and Rx are just the two halves of Px
                (lx, rx) = splitAt nD2 px

                -- The midpoint or median of Px
                -- Since n >= 4 in this branch, head is safe.
                x_median = head rx
                
                -- Ly and Ry need careful construction, done in makeLyRy 
                (ly, ry) = makeLyRy py x_median
            
                -- Recursively compute the closest pair in each half
                ls = closestPair lx ly
                rs = closestPair rx ry

                d_ls = dist ls
                d_rs = dist rs
                delta = min d_ls d_rs
                
                -- Find the closest split pair, if one exists
                ss = closestSplitPair py delta x_median

-- Input: two copies Px and Py of n >= 2 points in the
        -- plane, sorted by x- and y-coordinate (simplified here to just Py), and a
        -- parameter delta. (Added an x_median param. since it has already been computed).
-- Output: the closest pair, provided it is a split pair.
closestSplitPair :: [Point] -> Int -> Point -> Maybe (Int, Point, Point)
closestSplitPair py delta x_median =
        -- Filter the list to keep only points within delta of the median x-coordinate.
    let sy = filter (\p -> abs (x x_median - x p) <= delta) py in

        -- Start with no best point
        go sy Nothing

        where go [] best = best
        
              go (p:ps) Nothing =
                -- If there is no current best point, use whatever the brute search returns
                -- as the best
                go ps (bruteSearchSeven p ps)

              go (p:ps) best@(Just(bd, _, _)) =
                -- If there is a current best point, find the best pair for p and ps, and
                -- see if it is better than the current best.
                case bruteSearchSeven p ps of
                    new@(Just (d, _, _)) -> go ps (if d < bd then new else best)
                    Nothing -> go ps best

-- Searches either the next seven items in ps or the rest of the list, 
--    whichever is fewer, for the point that is closest to the given p.
bruteSearchSeven :: Point -> [Point] -> Maybe (Int, Point, Point)
bruteSearchSeven p ps =
    foldr (\p' best -> let new_dist = dist (p, p') in
                            case best of
                                -- If there is no current best pair, make this the new best.
                                Nothing -> Just (new_dist, p, p')
                                -- Otherwise, check if p' is closer to p than the current best, b.
                                b@(Just (bd', _, _)) -> if new_dist < bd'
                                                        then Just (new_dist, p, p')
                                                        else b)   
            Nothing -- Start with no point
            (take 7 ps) -- Only search up to the next seven entries

-- Performs linear scan through Py, putting each point into Ly or Ry.
makeLyRy :: [Point] -> Point -> ([Point], [Point])
makeLyRy [] _ = ([], [])
makeLyRy (p:ps) x_median =
    let (ly, ry) = makeLyRy ps x_median in
        if x p < x x_median then (p:ly, ry) else (ly, p:ry)

-- Computes the squared distance between two points
dist :: (Point, Point) -> Int
dist (p1, p2) = (x p1 - x p2) ^ (2 :: Int) + (y p1 - y p2) ^ (2 :: Int)

-- Sorts the list of points by their X-coordinates, least first.
sortByX :: [Point] -> [Point]
sortByX = S.sortBy (\p1 p2 -> compare (x p1) (x p2))

-- Sorts the list of points by their Y-coordinates, least first.
sortByY :: [Point] -> [Point]
sortByY = S.sortBy (\p1 p2 -> compare (y p1) (y p2))

