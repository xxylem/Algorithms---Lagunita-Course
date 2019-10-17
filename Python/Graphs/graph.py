class Graph:

    def __init__(self, edges=None, vertices=None):
        if vertices is None:
            vertices = []
        if edges is None:
            edges = []
        self.edges = set(edges)
        self.vertices = set(vertices)

    def get_edges(self):
        return self.edges

    def get_vertices(self):
        return self.vertices

    def add(self, obj):
        if isinstance(obj, Vertex):
            self.vertices.add(obj)
        if isinstance(obj, Edge):
            self.edges.add(obj)


class Vertex:

    def __init__(self, edges=None):
        if edges is None:
            edges = []
        self.edges = set(edges)

    def get_edges(self):
        return self.edges


class Edge:

    def __init__(self, node1, node2):
        if node1 == node2:
            raise ValueError

        self.node1 = node1
        self.node2 = node2
