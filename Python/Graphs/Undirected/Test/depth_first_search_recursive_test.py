import unittest

from Graphs.Undirected.Model.graph import Graph
from Graphs.Undirected.depth_first_search_recursive import depth_first_search_recursive


class DFSRecursiveTest(unittest.TestCase):

    def test_one_node_graph(self):
        g = Graph()
        v = g.add_vertex()

        explored = depth_first_search_recursive(g, v)
        expected = {
            v: True
        }

        self.assertEqual(expected, explored)

    def test_two_nodes_unconnected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()

        explored1 = depth_first_search_recursive(g, v1)
        expected1 = {
            v1: True,
            v2: False
        }

        explored2 = depth_first_search_recursive(g, v2)
        expected2 = {
            v1: False,
            v2: True
        }

        self.assertEqual(expected1, explored1)
        self.assertEqual(expected2, explored2)

    def test_two_nodes_connected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        g.add_edge(v1, v2)

        explored1 = depth_first_search_recursive(g, v1)
        expected1 = {
            v1: True,
            v2: True
        }

        explored2 = depth_first_search_recursive(g, v2)
        expected2 = {
            v1: True,
            v2: True
        }

        self.assertEqual(expected1, explored1)
        self.assertEqual(expected2, explored2)

    def test_on_larger_graph(self):
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

        explored_a = depth_first_search_recursive(grf, a)
        explored_b = depth_first_search_recursive(grf, b)
        explored_c = depth_first_search_recursive(grf, c)
        explored_d = depth_first_search_recursive(grf, d)
        explored_e = depth_first_search_recursive(grf, e)
        explored_g = depth_first_search_recursive(grf, g)
        explored_h = depth_first_search_recursive(grf, h)
        expected_all_but_f = {
            a: True,
            b: True,
            c: True,
            d: True,
            e: True,
            f: False,
            g: True,
            h: True
        }

        explored_f = depth_first_search_recursive(grf, f)
        expected_f = {
            a: False,
            b: False,
            c: False,
            d: False,
            e: False,
            f: True,
            g: False,
            h: False
        }

        self.assertEqual(expected_all_but_f, explored_a)
        self.assertEqual(expected_all_but_f, explored_b)
        self.assertEqual(expected_all_but_f, explored_c)
        self.assertEqual(expected_all_but_f, explored_d)
        self.assertEqual(expected_all_but_f, explored_e)
        self.assertEqual(expected_all_but_f, explored_g)
        self.assertEqual(expected_all_but_f, explored_h)

        self.assertEqual(expected_f, explored_f)


if __name__ == '__main__':
    unittest.main()
