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

    """ Add an edge or vertex to this graph."""
    # TODO recursively add all vertices and edges that are connected to the added component:
    #       This can be done once the graph algorithms are written, presumably.
    def add(self, obj):
        if isinstance(obj, Vertex):
            self.vertices.add(obj)
        if isinstance(obj, Edge):
            self.edges.add(obj)


class Vertex:
    """ A vertex, v, has a set of edges s.t. forall e in edges, e is incident to v. """

    def __init__(self, edges=None):
        if edges is None:
            edges = []
        self.edges = set(edges)

    def get_edges(self):
        return self.edges

    def add(self, edge):
        self.edges.add(edge)


class Edge:
    """ An edge, e, consists of the two vertices, vertex1 and vertex2, that are incident to the edge."""

    def __init__(self, vertex1, vertex2):
        if vertex1 == vertex2:
            raise ValueError

        self.vertex1 = vertex1
        self.vertex2 = vertex2

        vertex1.add(self)
        vertex2.add(self)

    def get_vertex1(self):
        return self.vertex1

    def get_vertex2(self):
        return self.vertex2
