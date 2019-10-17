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

    def test_create_larger_graph(self):
        grf = Graph()

        a = grf.add_vertex()
        b = grf.add_vertex()
        c = grf.add_vertex()
        d = grf.add_vertex()
        e = grf.add_vertex()
        f = grf.add_vertex()
        g = grf.add_vertex()
        h = grf.add_vertex()

        e1 = grf.add_edge(a, g)
        e2 = grf.add_edge(a, d)
        e3 = grf.add_edge(b, h)
        e4 = grf.add_edge(d, h)
        e5 = grf.add_edge(c, h)
        e6 = grf.add_edge(d, e)

        self.assertEqual(len(grf.get_vertices()), 8)
        self.assertEqual(grf.get_vertices(), {a, b, c, d, e, f, g, h})
        self.assertEqual(len(grf.get_edges()), 6)
        self.assertEqual(grf.get_edges(), {e1, e2, e3, e4, e5, e6})

        self.assertEqual(a.get_incident_edges(), {e1, e2})
        self.assertEqual(b.get_incident_edges(), {e3})
        self.assertEqual(c.get_incident_edges(), {e5})
        self.assertEqual(d.get_incident_edges(), {e2, e4, e6})
        self.assertEqual(e.get_incident_edges(), {e6})
        self.assertEqual(f.get_incident_edges(), set())
        self.assertEqual(g.get_incident_edges(), {e1})
        self.assertEqual(h.get_incident_edges(), {e3, e4, e5})

        self.assertEqual(e1.get_vertex1(), a)
        self.assertEqual(e1.get_vertex2(), g)
        self.assertEqual(e2.get_vertex1(), a)
        self.assertEqual(e2.get_vertex2(), d)
        self.assertEqual(e3.get_vertex1(), b)
        self.assertEqual(e3.get_vertex2(), h)
        self.assertEqual(e4.get_vertex1(), d)
        self.assertEqual(e4.get_vertex2(), h)
        self.assertEqual(e5.get_vertex1(), c)
        self.assertEqual(e5.get_vertex2(), h)
        self.assertEqual(e6.get_vertex1(), d)
        self.assertEqual(e6.get_vertex2(), e)

    def test_cannot_add_more_than_one_edge_between_two_nodes(self):

        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e = g.add_edge(v1, v2)
        self.assertEqual(g.get_edges(), {e})
        self.assertEqual(g.get_vertices(), {v1, v2})
        self.assertEqual(v1.get_incident_edges(), {e})
        self.assertEqual(v2.get_incident_edges(), {e})

        with self.assertRaises(ValueError):
            e2 = g.add_edge(v1, v2)

        with self.assertRaises(ValueError):
            e3 = g.add_edge(v2, v1)

        self.assertEqual(g.get_edges(), {e})
        self.assertEqual(g.get_vertices(), {v1, v2})
        self.assertEqual(v1.get_incident_edges(), {e})
        self.assertEqual(v2.get_incident_edges(), {e})


if __name__ == '__main__':
    unittest.main()
