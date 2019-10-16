package ListsAndArrays.Find;

import java.util.Arrays;
import java.util.Comparator;

public class ClosestPair {

    public static void main(String[] args) {

        // Example points
        Point p1 = new Point(2, 17);
        Point p2 = new Point(-50, 2000);
        Point p3 = new Point(9, 31);
        Point p4 = new Point(62, 63);
        Point p5 = new Point(63, 62);
        Point p6 = new Point(5048, 1);
        Point p7 = new Point(20, 99);
        Point p8 = new Point(824, 142);
        Point p9 = new Point(432523, 123);

        // Compute closest pair with nine example points.
        Point[] points = {p1, p2, p3, p4, p5, p6, p7, p8, p9};
        Point[] Px = sortByX(points);
        Point[] Py = sortByY(points);
        PointPair closest = closestPair(Px, Py);
        System.out.println(closest);

        // Compute closest pair with three example points.
        Point[] points2 = {p4, p5, p7};
        Point[] Px2 = sortByX(points2);
        Point[] Py2 = sortByY(points2);
        PointPair closest2 = closestPair(Px2, Py2);
        System.out.println(closest2);

    }

    /** Input: two copies Px and Py of n >= 2 points in the
            plane, sorted by x- and y-coordinate, respectively.
            Output: the pair pi, pj of distinct points with smallest
            Euclidean distance between them. */
    private static PointPair closestPair(Point[] Px, Point[] Py) {

        int n = Px.length;

        switch (n) {
            case 0:
                // Undefined on n < 2 points
                return null;

            case 1:
                // Undefined on n < 2 points
                return null;

            case 2:
                // Base case (n=2)
                // The closest pair of points with n = 2 points is just
                // those two points.
                Point p1 = Px[0];
                Point p2 = Px[1];
                return new PointPair(p1, p2);

            case 3:
                // Base case (n=3)
                // "Manually" compute the closest pair of the three points.
                p1 = Px[0];
                p2 = Px[1];
                Point p3 = Px[2];
                return closestOfThree(p1, p2, p3);

            default:
                // Inductive step (n>3)

                // Lx and Rx are just the left and right halves of Px
                int nD2 = n / 2;
                Point[] Lx = Arrays.copyOfRange(Px, 0, nD2);
                Point[] Rx = Arrays.copyOfRange(Px, nD2, n);

                // Linearly add points from Py to Ly and Ry depending
                // on if they are smaller or larger than the median.
                int x_median = Lx[Lx.length - 1].x;
                Point[] Ly = new Point[Lx.length];
                Point[] Ry = new Point[Rx.length];
                Point newPoint;
                for (int i = 0, j = 0, k = 0; i < n; i++) {
                    newPoint = Py[i];
                    if (newPoint.x <= x_median) {
                        Ly[j] = newPoint;
                        j++;
                    }
                    else {
                        Ry[k] = newPoint;
                        k++;
                    }
                }

                // Recursively compute the closest pair in Lx and Rx
                PointPair lp = closestPair(Lx, Ly);
                PointPair rp = closestPair(Rx, Ry);

                // Decide which is the closest pair of Lx and Rx
                // and set delta to be the distance squared of that pair.
                int delta;
                PointPair bestSidePair;
                if (lp.dist_sq < rp.dist_sq) {
                    delta = lp.dist_sq;
                    bestSidePair = lp;
                }
                else {
                    delta = rp.dist_sq;
                    bestSidePair = rp;
                }

                // Find the closest split pair, if there is one.
                PointPair sp = closestSplitPair(Py, delta, x_median);

                // Decide what the best overall pair is.
                PointPair bestPair;
                if (sp != null) {
                    if (sp.dist_sq < bestSidePair.dist_sq) {
                        bestPair = sp;
                    }
                    else {
                        bestPair = bestSidePair;
                    }
                }
                else {
                    bestPair = bestSidePair;
                }

                return bestPair;
        }
    }

    /** Input: two copies Px (removed since not needed) and Py of n >= 2 points in the
         plane, sorted by x- and y-coordinate, and a
         parameter delta. (also added x_median since it is already available.)
        Output: the closest pair, provided it is a split pair. */
    private static PointPair closestSplitPair(Point[] Py, int delta, int x_median) {

        int n = Py.length;

        // Filter Py -> Sy:
        //    Keep only Points that have x-coordinates within delta of x_median.
        Point[] tempArray = new Point[n];
        int k = 0;
        for (Point aPy : Py) {
            if (Math.abs(x_median - aPy.x) < delta) {
                tempArray[k] = aPy;
                k++;
            }
        }
        Point[] Sy = Arrays.copyOfRange(tempArray, 0, k);

        // Start with the best distance as delta and no best pair.
        int best = delta;
        PointPair bestPair = null;

        // Brute force search Sy for the best pair (if it exists).
        int l = Sy.length;
        for (int i = 0; i < l-1; i++){
            for (int j = 1; j < Math.min(8, l - i); j++) {
                PointPair temp = new PointPair(Sy[i], Sy[i+j]);
                if (temp.dist_sq < best) {
                    best = temp.dist_sq;
                    bestPair = temp;
                }
            }
        }
        return bestPair;
    }

    /** A point in the XY plane. */
    private static class Point {

        final int x;
        final int y;

        private Point (int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    /** A pair of Points in the XY plane and the distance
     *  squared between them. */
    private static class PointPair {

        final Point p1;
        final Point p2;
        final int dist_sq;

        private PointPair (Point p1, Point p2) {
            this.p1 = p1;
            this.p2 = p2;
            dist_sq = distance_squared(p1, p2);
        }

        public String toString() {
            String prtStr = "";
            prtStr += "Point 1: ( ";
            prtStr += Integer.toString(p1.x);
            prtStr += ", ";
            prtStr += Integer.toString(p1.y);
            prtStr += ")\nPoint 2: ( ";
            prtStr += Integer.toString(p2.x);
            prtStr += ", ";
            prtStr += Integer.toString(p2.y);
            prtStr += ")\n";
            prtStr += "Distance (squared) between points: ";
            prtStr += Integer.toString(dist_sq);
            return prtStr;
        }
    }

    /** Returns the pair of points that are closest out of three points */
    private static PointPair closestOfThree (Point p1, Point p2, Point p3) {

        // Compute the distances between each pair of the three points.
        int p12 = distance_squared(p1, p2);
        int p13 = distance_squared(p1, p3);
        int p23 = distance_squared(p2, p3);

        // Find the pair with the shortest distance.
        if (p12 < p13 && p12 < p23) {
            return new PointPair(p1, p2);
        }
        else if ( p13 < p23) {
            return new PointPair(p1, p3);
        }
        else {
            return new PointPair(p2, p3);
        }
    }

    /** Computes the squared distance between p1 and p2 */
    private static int distance_squared(Point p1, Point p2) {
        return (int) (Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2));
    }

    /** Sort a copy of the Point array by X-coordinates, smallest first. */
    private static Point[] sortByX(Point[] points) {
        // Take a copy of the array since we need to make Px and Py from points.
        Point[] Px = Arrays.copyOf(points, points.length);
        Arrays.sort(Px, new XComparator());
        return Px;
    }

    /** Sort a copy of the Point array by Y-coordinates, smallest first. */
    private static Point[] sortByY(Point[] points) {
        // Take a copy of the array since we need to make Px and Py from points.
        Point[] Py = Arrays.copyOf(points, points.length);
        Arrays.sort(Py, new YComparator());
        return Py;
    }

    /** Compares Points based on their X-coordinate. */
    private static class XComparator implements Comparator<Point> {
        @Override
        public int compare(Point p1, Point p2) {

            return Integer.compare(p1.x, p2.x);
        }
    }

    /** Compares Points based on their Y-coordinate. */
    private static class YComparator implements Comparator<Point> {
        @Override
        public int compare(Point p1, Point p2) {

            return Integer.compare(p1.y, p2.y);
        }
    }
}
