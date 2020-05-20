package dijkstra;

import java.util.Objects;

public class Edge {

    private final Vertex to;

    private final int length;

    public Edge(Vertex to, int length) {
        if (to == null) {
            throw new IllegalArgumentException("Vertices must be non-null");
        }
        if (length < 0) {
            throw new IllegalArgumentException("Length must be non-negative");
        }
        this.to = to;
        this.length = length;
    }

    public Vertex getTo() {
        return to;
    }

    public int getLength() {
        return length;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Edge edge = (Edge) o;
        return length == edge.length &&
                to.equals(edge.to);
    }

    @Override
    public int hashCode() {
        return Objects.hash(to, length);
    }
}
