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

    def add_edge(self, v1, v2):
        if v1 not in self.vertices or v2 not in self.vertices:
            raise ValueError("The vertices must already be in the graph.")
        if v1 == v2:
            raise ValueError("The vertices must be distinct.")
        if _edge_exists_from(v1, v2):
            raise ValueError("There is already a directed edge between those two vertices.")

        e = DirectedGraph.DirectedEdge(v1, v2)
        v1.get_incident_edges().add(e)
        self.edges.add(e)
        return e

    class DirectedEdge(Graph.Edge):

        def __init__(self, from_v, to_v):
            self.from_v = from_v
            self.to_v = to_v

        def get_from(self):
            return self.from_v

        def get_vertex1(self):
            return self.get_from()

        def get_to(self):
            return self.to_v

        def get_vertex2(self):
            return self.get_to()
