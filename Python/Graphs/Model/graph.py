def _edge_exists_between(v1, v2):
    """ Checks for an edge between the two given vertices.
        ASSUMES: v1 and v2 are distinct vertices.
                 v1 and v2 are in the same graph. """
    for e in v1.get_incident_edges():
        if e.get_vertex1() is v2 or e.get_vertex2() is v2:
            return True
    return False


class Graph:
    """ A simple representation of a graph G = (V, E) with the set of vertices V and set of edges E. """

    def __init__(self, edges=None, vertices=None):
        if vertices is None:
            vertices = []
        if edges is None:
            edges = []
        self.edges = set(edges)
        self.vertices = set(vertices)

    # Getters
    def get_edges(self): return self.edges
    def get_vertices(self): return self.vertices

    def add_vertex(self):
        """ Add an edge or vertex to this graph."""
        # TODO Do I want to add a label to the vertex? It may be easier to keep track of them in larger graphs.
        #      I could add nice printing with the label.
        v = Graph.Vertex()
        self.vertices.add(v)
        return v

    def add_edge(self, v1, v2):
        if v1 not in self.vertices or v2 not in self.vertices:
            raise ValueError("The vertices must already be in the graph.")
        if v1 == v2:
            raise ValueError("The vertices must be distinct.")
        if _edge_exists_between(v1, v2):
            raise ValueError("There is already an edge between those two vertices.")

        e = Graph.Edge(v1, v2)
        v1.get_incident_edges().add(e)
        v2.get_incident_edges().add(e)
        self.edges.add(e)
        return e

    class Vertex:
        """ A vertex, v, has a set of incident edges. """

        def __init__(self, edges=None):
            if edges is None:
                edges = []
            self.edges = set(edges)

        # Getter
        def get_incident_edges(self): return self.edges

    class Edge:
        """ An edge, e, consists of the two vertices, vertex1 and vertex2, that are incident to the edge."""

        def __init__(self, vertex1, vertex2):
            self.vertex1 = vertex1
            self.vertex2 = vertex2

        def get_vertex1(self):
            return self.vertex1

        def get_vertex2(self):
            return self.vertex2

