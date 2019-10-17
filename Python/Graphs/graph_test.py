import unittest

from Graphs.graph import Graph, Vertex, Edge


class GraphTest(unittest.TestCase):

    def test_create_empty_graph(self):
        g = Graph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())


class VertexTest(unittest.TestCase):

    def test_create_vertex_with_no_nodes(self):
        n = Vertex()
        self.assertEqual(n.get_edges(), set())


class EdgeTest(unittest.TestCase):

    def test_edge_must_be_incident_to_distinct_vertices(self):
        n = Vertex()
        with self.assertRaises(ValueError):
            Edge(n, n)

    def test_new_edge_adds_itself_to_incident_vertices(self):
        n1 = Vertex()
        n2 = Vertex()
        self.assertEqual(len(n1.get_edges()), 0)
        self.assertEqual(len(n2.get_edges()), 0)

        e = Edge(n1, n2)

        self.assertEqual(e.get_vertex1(), n1)
        self.assertEqual(e.get_vertex2(), n2)

        self.assertEqual(n1.get_edges(), {e})
        self.assertEqual(n2.get_edges(), {e})


if __name__ == '__main__':
    unittest.main()
