package ListsAndArrays.Find.ClosestPair;

/** A pair of Points in the XY plane and the distance
 *  squared between them. */
class PointPair {

    final Point p1;
    final Point p2;
    final int dist_sq;

    PointPair(Point p1, Point p2) {
        this.p1 = p1;
        this.p2 = p2;
        dist_sq = ClosestPair.distance_squared(p1, p2);
    }

    Point getP1() { return p1; }
    Point getP2() { return p2; }

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

    @Override
    public boolean equals(Object other) {

        if (this == other)
            return true;
        if (other == null)
            return false;
        if (getClass() != other.getClass())
            return false;
        PointPair pp = (PointPair) other;
        return (p1 == pp.p1 && p2 == pp.p2) || (p1 == pp.p2 && p2 == pp.p1);
    }
}
