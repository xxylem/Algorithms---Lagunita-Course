def _edge_exists_between(v1, v2):
    """ Checks for an edge between the two given vertices.
        ASSUMES: v1 and v2 are distinct vertices.
                 v1 and v2 are in the same graph. """
    for e in v1.get_incident_edges():
        if e.get_vertex1() is v2 or e.get_vertex2() is v2:
            return True
    return False


class Graph:
    """ A simple representation of a graph G = (V, E) with the set of vertices V and set of edges E.
        Standard usage:
            - create a graph: g = Graph()
            - add vertices: v1 = g.add_vertex(),...
            - add_edges: e = g.add_edge(v1, v2),...
        You can add names to the elements of the graph using the optional name parameter. """

    def __init__(self, name=None):
        if name is None:
            name = str(id(self))

        self.edges = set()
        self.vertices = set()
        self.name = name

    # Getters
    def get_vertex(self, name):
        """ Get a vertex by its name. """
        for v in self.vertices:
            if v.name == name:
                return v
        return None

    def get_edge(self, from_v, to_v):
        """ Get a vertex based on the vertices incident to it. """
        for e in self.edges:
            if e.get_vertex1() == from_v:
                if e.get_vertex2() == to_v:
                    return e
        return None

    def get_name(self): return self.name
    def get_edges(self): return self.edges
    def get_vertices(self): return self.vertices
    # End getters

    def add_vertex(self, name=None):
        """ Add a new vertex to this graph."""
        # TODO prevent adding a vertex with a name already taken.
        v = Graph.Vertex(name=name)
        self.vertices.add(v)
        return v

    def add_edge(self, v1, v2, name=None):
        """ Add an undirected edge between v1 and v2. """
        # TODO prevent adding an edge with a name already taken.
        if v1 not in self.vertices or v2 not in self.vertices:
            raise ValueError("The vertices must already be in the graph.")
        if v1 == v2:
            raise ValueError("The vertices must be distinct.")
        if _edge_exists_between(v1, v2):
            raise ValueError("There is already an edge between those two vertices.")

        e = Graph.Edge(v1, v2, name)
        v1.get_incident_edges().add(e)
        v2.get_incident_edges().add(e)
        self.edges.add(e)
        return e

    class Vertex:
        """ A vertex, v, has a set of incident edges. """

        def __init__(self, name=None):
            if name is None:
                name = str(id(self))

            self.edges = set()
            self.name = name

        # Getters
        def get_name(self): return self.name
        def get_incident_edges(self): return self.edges

        def get_neighbours(self):
            """ Get all the neighbours of this vertex"""
            neighbours = []
            for ie in self.edges:
                v1 = ie.get_vertex1()
                v2 = ie.get_vertex2()
                if self is v1:
                    new_v = v2
                else:
                    new_v = v1
                neighbours.append(new_v)
            return neighbours

        # Overidden core object methods.
        def __eq__(self, other):
            if isinstance(other, Graph.Vertex):
                return self.name == other.name
            return False

        def __hash__(self): return hash(self.name)

        def __repr__(self): return self.name

    class Edge:
        """ An edge, e, consists of the two vertices, vertex1 and vertex2, that are incident to the edge."""

        def __init__(self, vertex1, vertex2, name=None):
            if name is None:
                name = str(id(self))

            self.vertex1 = vertex1
            self.vertex2 = vertex2
            self.name = name

        # Getters
        def get_name(self): return self.name
        def get_vertex1(self): return self.vertex1
        def get_vertex2(self): return self.vertex2

        def __eq__(self, other):
            if isinstance(other, Graph.Edge):
                return self.name == other.name and self.vertex1 == other.vertex1 and self.vertex2 == other.vertex2
            return False

        def __hash__(self):
            return hash((self.name, self.vertex1, self.vertex2))

        def __repr__(self):
            return str(self.vertex1) + " ===== " + self.name + " ====> " + str(self.vertex2)
