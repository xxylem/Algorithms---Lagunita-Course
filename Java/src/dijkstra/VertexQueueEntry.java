package dijkstra;

import java.util.Objects;

public class VertexQueueEntry {

    private int key;

    private final Vertex v;

    public VertexQueueEntry(int key, Vertex v)
    {
        this.key = key;
        this.v = v;
    }

    public int getKey() {
        return key;
    }

    public void setKeyMin(int other) {
        key = Math.min(key, other);
    }

    public Vertex getV() {
        return v;
    }

    @Override
    public boolean equals(Object o) {
        // this.equals(o) when the vertices are the same. Needed to be able to remove.
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VertexQueueEntry entry = (VertexQueueEntry) o;
        return v.equals(entry.v);
    }

    @Override
    public int hashCode() {
        return Objects.hash(v);
    }
}
