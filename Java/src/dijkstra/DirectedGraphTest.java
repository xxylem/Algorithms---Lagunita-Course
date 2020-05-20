package dijkstra;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DirectedGraphTest {

    private DirectedGraph directedGraph;

    private Vertex a;
    private Vertex b;
    private Vertex c;
    private Vertex d;
    private Vertex e;
    private Vertex f;
    private Vertex g;
    private Vertex h;

    private List<Integer> expectedAtoH;

    @BeforeEach
    public void setup()
    {
        directedGraph = new DirectedGraph();
        directedGraph.addVertex("A");
        directedGraph.addVertex("B");
        directedGraph.addVertex("C");
        directedGraph.addVertex("D");
        directedGraph.addVertex("E");
        directedGraph.addVertex("F");
        directedGraph.addVertex("G");
        directedGraph.addVertex("H");

        directedGraph.addEdge("A", "B", 54);
        directedGraph.addEdge("B", "A", 200);
        directedGraph.addEdge("B", "C", 20);
        directedGraph.addEdge("B", "E", 5);
        directedGraph.addEdge("C", "D", 4);
        directedGraph.addEdge("D", "A", 5);
        directedGraph.addEdge("D", "G", 5);
        directedGraph.addEdge("H", "C", 3);

        // Init them here instead of using them in the directedgraph
        // to indirectly check equals/hash code
        a = new Vertex("A");
        b = new Vertex("B");
        c = new Vertex("C");
        d = new Vertex("D");
        e = new Vertex("E");
        f = new Vertex("F");
        g = new Vertex("G");
        h = new Vertex("H");
    }


    @Test
    public void testPathsFromA()
    {
        expectedAtoH = List.of(0, 54, 74, 78, 59, Integer.MAX_VALUE, 83, Integer.MAX_VALUE);
        assertDistances(a);
    }

    @Test
    public void testPathsFromB()
    {
        expectedAtoH = List.of(29, 0, 20, 24, 5, Integer.MAX_VALUE, 29, Integer.MAX_VALUE);
        assertDistances(b);
    }

    @Test
    public void testPathsFromC()
    {
        expectedAtoH = List.of(9, 63, 0, 4, 68, Integer.MAX_VALUE, 9, Integer.MAX_VALUE);
        assertDistances(c);
    }

    @Test
    public void testPathsFromD()
    {
        expectedAtoH = List.of(5, 59, 79, 0, 64, Integer.MAX_VALUE, 5, Integer.MAX_VALUE);
        assertDistances(d);
    }

    @Test
    public void testPathsFromE()
    {
        expectedAtoH = List.of(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, 0,
                Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE);
        assertDistances(e);
    }

    @Test
    public void testPathsFromF()
    {
        expectedAtoH = List.of(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE,
                Integer.MAX_VALUE, 0, Integer.MAX_VALUE, Integer.MAX_VALUE);
        assertDistances(f);
    }

    @Test
    public void testPathsFromG()
    {
        expectedAtoH = List.of(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE,
                Integer.MAX_VALUE, Integer.MAX_VALUE, 0, Integer.MAX_VALUE);
        assertDistances(g);
    }

    @Test
    public void testPathsFromH()
    {
        expectedAtoH = List.of(12 , 66, 3, 7, 71, Integer.MAX_VALUE, 12, 0);
        assertDistances(h);
    }

    private void assertDistances(Vertex from)
    {
        Map<Vertex, Integer> expected = new HashMap<>();
        expected.put(a, expectedAtoH.get(0));
        expected.put(b, expectedAtoH.get(1));
        expected.put(c, expectedAtoH.get(2));
        expected.put(d, expectedAtoH.get(3));
        expected.put(e, expectedAtoH.get(4));
        expected.put(f, expectedAtoH.get(5));
        expected.put(g, expectedAtoH.get(6));
        expected.put(h, expectedAtoH.get(7));

        assertEquals(expected, directedGraph.shortestPathDistancesDijkstra(from));
    }

}