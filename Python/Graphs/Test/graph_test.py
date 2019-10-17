import unittest

from Graphs.Model.graph import Graph


class GraphTest(unittest.TestCase):

    def test_create_empty_graph(self):
        g = Graph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())

    def test_create_graph_with_one_vertex(self):
        g = Graph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())
        v = g.add_vertex()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), {v})

    def test_create_graph_only_vertices(self):
        g = Graph()
        vs = []
        for v in range(10):
            vs.append(g.add_vertex())
        self.assertEqual(len(vs), len(set(vs)))
        self.assertEqual(g.get_vertices(), set(vs))

    def test_simplest_two_vertices_one_edge(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e = g.add_edge(v1, v2)
        self.assertEqual(g.get_edges(), {e})
        self.assertEqual(g.get_vertices(), {v1, v2})

    def test_cannot_add_an_edge_with_node_not_in_the_graph(self):
        g1 = Graph()
        v_in_g1 = g1.add_vertex()

        g2 = Graph()
        v_in_g2 = g2.add_vertex()

        with self.assertRaises(ValueError):
            g1.add_edge(v_in_g1, v_in_g2)

        with self.assertRaises(ValueError):
            g2.add_edge(v_in_g1, v_in_g2)

    def test_cannot_add_edges_to_nodes_both_not_in_the_graph(self):
        g1 = Graph()
        v1 = g1.add_vertex()
        v2 = g1.add_vertex()

        g2 = Graph()

        with self.assertRaises(ValueError):
            g2.add_edge(v1, v2)


if __name__ == '__main__':
    unittest.main()
