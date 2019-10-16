import Test.Hspec (Spec, describe, it, anyException, shouldThrow, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Control.Exception (evaluate)

import qualified ClosestPair as C

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    describe "Tests that closestPair finds the closest pair of points" $ do

        it "Less than two points should throw exception" $ do
            evaluate (C.closestPair [] []) `shouldThrow` anyException
            evaluate (C.closestPair [pt1] [pt1]) `shouldThrow` anyException

        it "closest pair of two points is those two points" $ do
            pToL (C.closestPair twoPointsX twoPointsY) `shouldMatchList` [pt3, pt7]
        
        it "closest pair of all points" $ do
            pToL (C.closestPair allPointsX allPointsY) `shouldMatchList` [pt7, pt9]
    where pToL (a, b) = [a, b]


-- Example points
pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8, pt9 :: C.Point
pt1 = C.Point 2 17
pt2 = C.Point (-3) 2
pt3 = C.Point 18 8
pt4 = C.Point 27 0
pt5 = C.Point 4 6
pt6 = C.Point 9 12
pt7 = C.Point 16 17
pt8 = C.Point 1000 99
pt9 = C.Point 17 16

-- Lists of example points
twoPoints, twoPointsX, twoPointsY :: [C.Point]
twoPoints = [pt3, pt7]
twoPointsX = C.sortByX twoPoints
twoPointsY = C.sortByY twoPoints

allPoints, allPointsX, allPointsY :: [C.Point]
allPoints = [pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8, pt9]
-- Same list, sorted by X-coordinates
allPointsX = C.sortByX allPoints
-- Same list, sorted by Y-coordinates
allPointsY = C.sortByY allPoints
