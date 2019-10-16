package ListsAndArrays.Find.ClosestPair;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static ListsAndArrays.Find.ClosestPair.ClosestPair.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ClosestPairTest {

    private Point p1, p2, p3, p4, p5, p6, p7, p8, p9;

    @BeforeEach
    void setup() {
        p1 = new Point(2, 17);
        p2 = new Point(-50, 2000);
        p3 = new Point(9, 31);
        p4 = new Point(62, 63);
        p5 = new Point(63, 62);
        p6 = new Point(5048, 1);
        p7 = new Point(20, 99);
        p8 = new Point(824, 142);
        p9 = new Point(432523, 123);
    }

    @Test
    void closestPairWithNinePoints() {
        Point[] points = {p1, p2, p3, p4, p5, p6, p7, p8, p9};
        Point[] Px = sortByX(points);
        Point[] Py = sortByY(points);
        PointPair closest = closestPair(Px, Py);
        PointPair expected = new PointPair(p4, p5);

        assertEquals(closest, expected);
    }

    @Test
    void closestPairWithThreePoints() {
        Point[] points = {p4, p5, p7};
        Point[] Px = sortByX(points);
        Point[] Py = sortByY(points);
        PointPair closest = closestPair(Px, Py);

        PointPair expected = new PointPair(p4, p5);

        assertEquals(closest, expected);

    }
}
