from Graphs.Undirected.Model.graph import Graph


def _edge_exists_from(v1, v2):
    """ Checks if there is a directed edge from v1 to v2.
        ASSUMES: v1 and v2 are distinct vertices.
                 v1 and v2 are in the same graph.
                 v1 only has a list of outgoing edges. """
    for e in v1.get_incident_edges():
        if e.get_to() is v2:
            return True
    return False


class DirectedGraph(Graph):

    def add_vertex(self, name=None):
        """ Add a new vertex to this directed graph."""
        # TODO prevent adding a vertex with a name already taken.
        v = DirectedGraph.Vertex(name=name)
        self.vertices.add(v)
        return v

    def add_edge(self, v1, v2, name=None):
        """ Add a directed edge from v1 to v2."""

        if v1 not in self.vertices or v2 not in self.vertices:
            raise ValueError("The vertices must already be in the graph.")
        if v1 == v2:
            raise ValueError("The vertices must be distinct.")
        if _edge_exists_from(v1, v2):
            raise ValueError("There is already a directed edge between those two vertices.")

        e = DirectedGraph.DirectedEdge(v1, v2, name=name)
        v1.get_incident_edges().add(e)
        self.edges.add(e)
        return e

    def reverse(self):
        """ Returns a copy of the digraph with a copy of all vertices and an edge in the opposite direction
            to the original.
            NOTE: Creates new vertices to avoid destructive effects to the original graph.
                  Use the get_vertex() and get_edge() methods to search for the equivalent
                  vertex/edge by name. """

        rev_g = DirectedGraph(name=self.name + " reversed")
        names_to_vertices = {}

        # Copy over vertices using the same names as in the original graph.
        for v in self.vertices:
            new_v = rev_g.add_vertex(name=v.get_name())
            names_to_vertices[new_v.get_name()] = new_v

        # Establish edges in the reverse direction between vertices using their names.
        for e in self.edges:
            old_from_name = e.get_from().get_name()
            new_to_vertex = names_to_vertices[old_from_name]
            old_to_name = e.get_to().get_name()
            new_from_vertex = names_to_vertices[old_to_name]
            rev_g.add_edge(new_from_vertex, new_to_vertex, name=e.get_name() + " reversed")

        return rev_g

    class DirectedVertex(Graph.Vertex):
        def get_neighbours(self):
            """ Get all the neighbours of this vertex"""
            neighbours = []
            for ie in self.edges:
                neighbours.append(ie.get_to())
            return neighbours

    class DirectedEdge(Graph.Edge):

        def get_from(self):
            return self.get_vertex1()

        def get_to(self):
            return self.get_vertex2()
