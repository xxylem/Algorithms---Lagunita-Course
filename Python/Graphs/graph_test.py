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


if __name__ == '__main__':
    unittest.main()
