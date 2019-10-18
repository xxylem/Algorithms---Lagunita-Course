import unittest

from Graphs.Directed.Model.digraph import DirectedGraph, _edge_exists_from


class DirectedGraphTest(unittest.TestCase):

    def test_create_empty_graph(self):
        g = DirectedGraph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())

    def test_create_graph_with_one_vertex(self):
        g = DirectedGraph()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), set())
        v = g.add_vertex()
        self.assertEqual(g.get_edges(), set())
        self.assertEqual(g.get_vertices(), {v})

    def test_get_vertex_by_name(self):
        g = DirectedGraph()
        v = g.add_vertex(name="BOB")
        found_v = g.get_vertex("BOB")
        self.assertIsNotNone(found_v)
        self.assertEqual(v, found_v)

    def test_get_edge_by_vertices(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e = g.add_edge(v1, v2, name="LOVELY WALK")
        found_e = g.get_edge(v1, v2)
        self.assertIsNotNone(found_e)
        self.assertEqual(e, found_e)

    def test_create_graph_only_vertices(self):
        g = DirectedGraph()
        vs = []
        for v in range(10):
            vs.append(g.add_vertex())
        self.assertEqual(len(vs), len(set(vs)))
        self.assertEqual(g.get_vertices(), set(vs))
        self.assertEqual(len(g.get_edges()), 0)

    def test_simplest_two_vertices_one_edge(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e = g.add_edge(v1, v2)
        self.assertEqual(g.get_edges(), {e})
        self.assertEqual(g.get_vertices(), {v1, v2})
        self.assertEqual(e.get_from(), v1)
        self.assertEqual(e.get_to(), v2)
        self.assertNotEqual(e.get_to(), v1)
        self.assertNotEqual(e.get_from(), v2)
        self.assertEqual(v1.get_incident_edges(), {e})
        self.assertEqual(v2.get_incident_edges(), set())

    def test_simplest_digraph_two_vertices_one_edge_each_direction(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e1 = g.add_edge(v1, v2)
        e2 = g.add_edge(v2, v1)
        self.assertEqual(g.get_edges(), {e1, e2})
        self.assertEqual(g.get_vertices(), {v1, v2})
        self.assertNotEqual(v1, v2)
        self.assertEqual(e1.get_from(), v1)
        self.assertEqual(e1.get_to(), v2)
        self.assertEqual(e2.get_from(), v2)
        self.assertEqual(e2.get_to(), v1)
        self.assertEqual(v1.get_incident_edges(), {e1})
        self.assertEqual(v2.get_incident_edges(), {e2})

    def test_cannot_add_an_edge_with_node_not_in_the_graph(self):
        g1 = DirectedGraph()
        v_in_g1 = g1.add_vertex()

        g2 = DirectedGraph()
        v_in_g2 = g2.add_vertex()

        with self.assertRaises(ValueError):
            g1.add_edge(v_in_g1, v_in_g2)

        with self.assertRaises(ValueError):
            g1.add_edge(v_in_g2, v_in_g1)

        with self.assertRaises(ValueError):
            g2.add_edge(v_in_g1, v_in_g2)

        with self.assertRaises(ValueError):
            g2.add_edge(v_in_g2, v_in_g1)

    def test_cannot_add_edges_to_nodes_both_not_in_the_graph(self):
        g1 = DirectedGraph()
        v1 = g1.add_vertex()
        v2 = g1.add_vertex()

        g2 = DirectedGraph()

        with self.assertRaises(ValueError):
            g2.add_edge(v1, v2)

        with self.assertRaises(ValueError):
            g2.add_edge(v2, v1)

    def test_create_larger_graph(self):
        grf = DirectedGraph()

        a = grf.add_vertex()
        b = grf.add_vertex()
        c = grf.add_vertex()
        d = grf.add_vertex()
        e = grf.add_vertex()
        f = grf.add_vertex()
        g = grf.add_vertex()
        h = grf.add_vertex()
        i = grf.add_vertex()
        j = grf.add_vertex()

        e_a_g = grf.add_edge(a, g)
        e_a_d = grf.add_edge(a, d)
        e_d_a = grf.add_edge(d, a)
        e_d_h = grf.add_edge(d, h)
        e_d_e = grf.add_edge(d, e)
        e_e_d = grf.add_edge(e, d)
        e_h_b = grf.add_edge(h, b)
        e_c_h = grf.add_edge(c, h)
        e_i_j = grf.add_edge(i, j)

        self.assertEqual(len(grf.get_vertices()), 10)
        self.assertEqual(grf.get_vertices(), {a, b, c, d, e, f, g, h, i, j})
        self.assertEqual(len(grf.get_edges()), 9)
        self.assertEqual(grf.get_edges(), {e_a_g, e_a_d, e_d_a, e_d_h, e_d_e, e_e_d, e_h_b, e_c_h, e_i_j})

        self.assertEqual(a.get_incident_edges(), {e_a_g, e_a_d})
        self.assertEqual(b.get_incident_edges(), set())
        self.assertEqual(c.get_incident_edges(), {e_c_h})
        self.assertEqual(d.get_incident_edges(), {e_d_a, e_d_h, e_d_e})
        self.assertEqual(e.get_incident_edges(), {e_e_d})
        self.assertEqual(f.get_incident_edges(), set())
        self.assertEqual(g.get_incident_edges(), set())
        self.assertEqual(h.get_incident_edges(), {e_h_b})
        self.assertEqual(i.get_incident_edges(), {e_i_j})
        self.assertEqual(j.get_incident_edges(), set())

        self.assertEqual(e_a_g.get_from(), a)
        self.assertEqual(e_a_g.get_to(), g)
        self.assertEqual(e_a_d.get_from(), a)
        self.assertEqual(e_a_d.get_to(), d)
        self.assertEqual(e_d_a.get_from(), d)
        self.assertEqual(e_d_a.get_to(), a)
        self.assertEqual(e_d_h.get_from(), d)
        self.assertEqual(e_d_h.get_to(), h)
        self.assertEqual(e_d_e.get_from(), d)
        self.assertEqual(e_d_e.get_to(), e)
        self.assertEqual(e_e_d.get_from(), e)
        self.assertEqual(e_e_d.get_to(), d)
        self.assertEqual(e_h_b.get_from(), h)
        self.assertEqual(e_h_b.get_to(), b)
        self.assertEqual(e_c_h.get_from(), c)
        self.assertEqual(e_c_h.get_to(), h)
        self.assertEqual(e_i_j.get_from(), i)
        self.assertEqual(e_i_j.get_to(), j)

    def test_cannot_add_more_than_one_edge_between_two_nodes_same_direction(self):

        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        e1 = g.add_edge(v1, v2)
        self.assertEqual(g.get_edges(), {e1})
        self.assertEqual(g.get_vertices(), {v1, v2})
        self.assertEqual(v1.get_incident_edges(), {e1})
        self.assertEqual(v2.get_incident_edges(), set())
        self.assertEqual(e1.get_from(), v1)
        self.assertEqual(e1.get_to(), v2)

        with self.assertRaises(ValueError):
            bad_e1 = g.add_edge(v1, v2)

        e2 = g.add_edge(v2, v1)
        self.assertEqual(g.get_edges(), {e1, e2})
        self.assertEqual(v1.get_incident_edges(), {e1})
        self.assertEqual(v2.get_incident_edges(), {e2})
        self.assertEqual(e2.get_from(), v2)
        self.assertEqual(e2.get_to(), v1)

        with self.assertRaises(ValueError):
            bad_e2 = g.add_edge(v2, v1)

        self.assertEqual(g.get_edges(), {e1, e2})

    def test_reverse_doesnt_mutate_original_graph(self):
        orig_g = DirectedGraph()
        a = orig_g.add_vertex()
        b = orig_g.add_vertex()
        e = orig_g.add_edge(a, b)
        orig_g.reverse()
        self.assertEqual(orig_g.get_vertices(), {a, b})
        self.assertEqual(orig_g.get_edges(), {e})
        self.assertEqual(a.get_incident_edges(), {e})
        self.assertEqual(b.get_incident_edges(), set())

    def test_reverse_edge_in_two_vertex_graph(self):
        orig_g = DirectedGraph()
        a = orig_g.add_vertex(name="A")
        b = orig_g.add_vertex(name="B")
        e_a_b = orig_g.add_edge(a, b, name="AB")
        g_rev = orig_g.reverse()

        self.assertEqual(len(g_rev.get_edges()), 1)
        self.assertEqual(len(g_rev.get_vertices()), 2)

        a_rev = g_rev.get_vertex("A")
        self.assertIsNotNone(a_rev)
        b_rev = g_rev.get_vertex("B")
        self.assertIsNotNone(b_rev)

        self.assertEqual(a_rev, a)
        self.assertEqual(b_rev, b)

        self.assertTrue(_edge_exists_from(b_rev, a_rev))
        self.assertFalse(_edge_exists_from(a_rev, b_rev))

        e_b_a = g_rev.get_edge(b_rev, a_rev)
        self.assertIsNotNone(e_b_a)
        self.assertIsNone(g_rev.get_edge(a_rev, b_rev))
        self.assertNotEqual(e_a_b, e_b_a)


if __name__ == '__main__':
    unittest.main()
