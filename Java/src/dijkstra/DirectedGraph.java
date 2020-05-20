package dijkstra;

import java.util.*;

public class DirectedGraph {

    private final Map<Vertex, Set<Edge>> verticesToEdges;

    public DirectedGraph() {
        verticesToEdges = new HashMap<>();
    }

    public void addVertex(String label) {
        Vertex v = new Vertex(label);
        addVertex(v);
    }

    public void addVertex(Vertex v) {
        if (verticesToEdges.containsKey(v)) {
            throw new IllegalArgumentException(
                    String.format("Vertex already exists with label [%s]", v.getLabel()));
        }
        verticesToEdges.put(v, new HashSet<>());
    }

    public void addEdge(String from, String to, int length) {
        Vertex fromV = new Vertex(from);
        Vertex toV = new Vertex(to);
        addEdge(fromV, toV, length);
    }

    public void addEdge(Vertex from, Vertex to, int length) {
        Edge e = new Edge(to, length);
        addEdge(from, e);
    }

    public void addEdge(Vertex from, Edge e) {
        if (!verticesToEdges.containsKey(from) || !verticesToEdges.containsKey(e.getTo())) {
            throw new IllegalArgumentException("Check if vertices exist in graph");
        }

        if (verticesToEdges.get(from).contains(e)) {
            throw new IllegalArgumentException("Edge already exists between those vertices.");
        }

        verticesToEdges.get(from).add(e);
    }

    public boolean containsVertex(Vertex v) {
        if (v == null) {
            throw new IllegalArgumentException("Vertex must not be null");
        }
        return verticesToEdges.containsKey(v);
    }

    public Map<Vertex, Integer> shortestPathDistancesDijkstra(String start) {
        return shortestPathDistancesDijkstra(new Vertex(start));
    }

    public Map<Vertex, Integer> shortestPathDistancesDijkstra(Vertex start) {

        if (!containsVertex(start)) {
            throw new IllegalArgumentException("Start vertex not in directed graph");
        }

        // X := empty set, H := empty heap
        Set<Vertex> verticesInX = new HashSet<>();
        PriorityQueue<VertexQueueEntry> heap = getHeapStartingFrom(start);
        Map<Vertex, Integer> distances = getStartingDistances();

        // 3 H is non-empty do
        while (!heap.isEmpty()) {

            VertexQueueEntry wStar = heap.poll();
            verticesInX.add(wStar.getV());
            distances.put(wStar.getV(), wStar.getKey());

            Set<Edge> edgesFromWStar = verticesToEdges.get(wStar.getV());
            for (Edge e : edgesFromWStar)
            {
                Vertex y = e.getTo();
                if (verticesInX.contains(y))
                {
                    continue;
                }
                heap.removeIf(vqe -> vqe.getV().equals(y)); // TODO probably not O(ln n)
                int dijkstraScore = (int) Math.min((long)distances.get(y), distances.get(wStar.getV()) + (long)e.getLength());
                distances.put(y, dijkstraScore);
                VertexQueueEntry yEntry = new VertexQueueEntry(dijkstraScore, y);
                heap.add(yEntry);
            }
        }

        return distances;
    }

    private PriorityQueue<VertexQueueEntry> getHeapStartingFrom(Vertex start)
    {
        PriorityQueue<VertexQueueEntry> heap = new PriorityQueue<>(verticesToEdges.size(), Comparator.comparing(VertexQueueEntry::getKey));
        for (Vertex v : verticesToEdges.keySet())
        {
            VertexQueueEntry entry = new VertexQueueEntry(Integer.MAX_VALUE, v);
            if (v.equals(start))
            {
                 entry.setKeyMin(0);
            }
            heap.add(entry);
        }
        return heap;
    }

    private Map<Vertex, Integer> getStartingDistances() {
        Map<Vertex, Integer> distances = new HashMap<>();
        for (Vertex v : verticesToEdges.keySet())
        {
                distances.put(v, Integer.MAX_VALUE);
        }
        return distances;
    }
}
