package ListsAndArrays.Find.ClosestPair;

import java.util.Comparator;

/** A point in the XY plane. */
class Point {

    final int x;
    final int y;

    Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other)
            return true;
        if (other == null)
            return false;
        if (getClass() != other.getClass())
            return false;
        Point point = (Point) other;
        return (x == point.x && y == point.y);
    }

    /** Compares Points based on their X-coordinate. */
    static class XComparator implements Comparator<Point> {
        @Override
        public int compare(Point p1, Point p2) {

            return Integer.compare(p1.x, p2.x);
        }
    }

    /** Compares Points based on their Y-coordinate. */
    static class YComparator implements Comparator<Point> {
        @Override
        public int compare(Point p1, Point p2) {

            return Integer.compare(p1.y, p2.y);
        }
    }
}
