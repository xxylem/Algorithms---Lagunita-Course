import unittest

from Graphs.Model.graph import Graph, Vertex, Edge


class GraphTest(unittest.TestCase):

    def test_create_empty_graph(self):
        g = Graph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())

    def test_create_graph_with_one_vertex(self):
        g = Graph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())
        v = Vertex()
        g.add(v)
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), {v})

    def test_create_graph_only_vertices(self):
        g = Graph()
        vs = set()
        for _ in range(10):
            vs.add(Vertex())
        for v in vs:
            g.add(v)
        self.assertEqual(g.get_vertices(), vs)

    def test_simplest_two_vertices_one_edge(self):
        g = Graph()
        v1 = Vertex()
        v2 = Vertex()
        e = Edge(v1, v2)
        g.add(v1)
        g.add(v2)
        g.add(e)
        self.assertEqual(g.get_edges(), {e})
        self.assertEqual(g.get_vertices(), {v1, v2})


class VertexTest(unittest.TestCase):

    def test_create_vertex_with_no_nodes(self):
        v = Vertex()
        self.assertEqual(v.get_edges(), set())


class EdgeTest(unittest.TestCase):

    def test_edge_must_be_incident_to_distinct_vertices(self):
        v = Vertex()
        with self.assertRaises(ValueError):
            Edge(v, v)

    def test_new_edge_adds_itself_to_incident_vertices(self):
        v1 = Vertex()
        v2 = Vertex()
        self.assertEqual(len(v1.get_edges()), 0)
        self.assertEqual(len(v2.get_edges()), 0)

        e = Edge(v1, v2)

        self.assertEqual(e.get_vertex1(), v1)
        self.assertEqual(e.get_vertex2(), v2)

        self.assertEqual(v1.get_edges(), {e})
        self.assertEqual(v2.get_edges(), {e})


if __name__ == '__main__':
    unittest.main()
