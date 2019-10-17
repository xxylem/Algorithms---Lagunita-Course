import math
import unittest

from Graphs.Model.graph import Graph
from Graphs.shortest_path_breadth_first_search import shortest_path_breadth_first_search


class BreadFirstSearchTest(unittest.TestCase):

    def test_one_node_graph(self):
        g = Graph()
        v = g.add_vertex()

        distances = shortest_path_breadth_first_search(g, v)
        expected = {
            v: 0
        }

        self.assertEqual(expected, distances)

    def test_two_nodes_unconnected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()

        distances1 = shortest_path_breadth_first_search(g, v1)
        expected1 = {
            v1: 0,
            v2: math.inf
        }

        distances2 = shortest_path_breadth_first_search(g, v2)
        expected2 = {
            v1: math.inf,
            v2: 0
        }

        self.assertEqual(expected1, distances1)
        self.assertEqual(expected2, distances2)

    def test_two_nodes_connected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        g.add_edge(v1, v2)

        distances1 = shortest_path_breadth_first_search(g, v1)
        expected1 = {
            v1: 0,
            v2: 1
        }

        distances2 = shortest_path_breadth_first_search(g, v2)
        expected2 = {
            v1: 1,
            v2: 0
        }

        self.assertEqual(expected1, distances1)
        self.assertEqual(expected2, distances2)

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

        distances_a = shortest_path_breadth_first_search(grf, a)
        distances_b = shortest_path_breadth_first_search(grf, b)
        distances_c = shortest_path_breadth_first_search(grf, c)
        distances_d = shortest_path_breadth_first_search(grf, d)
        distances_e = shortest_path_breadth_first_search(grf, e)
        distances_f = shortest_path_breadth_first_search(grf, f)
        distances_g = shortest_path_breadth_first_search(grf, g)
        distances_h = shortest_path_breadth_first_search(grf, h)

        expected_a = {
            a: 0,
            b: 3,
            c: 3,
            d: 1,
            e: 2,
            f: math.inf,
            g: 1,
            h: 2
        }
        expected_b = {
            a: 3,
            b: 0,
            c: 2,
            d: 2,
            e: 3,
            f: math.inf,
            g: 4,
            h: 1
        }
        expected_c = {
            a: 3,
            b: 2,
            c: 0,
            d: 2,
            e: 3,
            f: math.inf,
            g: 4,
            h: 1
        }
        expected_d = {
            a: 1,
            b: 2,
            c: 2,
            d: 0,
            e: 1,
            f: math.inf,
            g: 2,
            h: 1
        }
        expected_e = {
            a: 2,
            b: 3,
            c: 3,
            d: 1,
            e: 0,
            f: math.inf,
            g: 3,
            h: 2
        }
        expected_f = {
            a: math.inf,
            b: math.inf,
            c: math.inf,
            d: math.inf,
            e: math.inf,
            f: 0,
            g: math.inf,
            h: math.inf
        }
        expected_g = {
            a: 1,
            b: 4,
            c: 4,
            d: 2,
            e: 3,
            f: math.inf,
            g: 0,
            h: 3
        }
        expected_h = {
            a: 2,
            b: 1,
            c: 1,
            d: 1,
            e: 2,
            f: math.inf,
            g: 3,
            h: 0
        }

        self.assertEqual(expected_a, distances_a)
        self.assertEqual(expected_b, distances_b)
        self.assertEqual(expected_c, distances_c)
        self.assertEqual(expected_d, distances_d)
        self.assertEqual(expected_e, distances_e)
        self.assertEqual(expected_g, distances_g)
        self.assertEqual(expected_f, distances_f)
        self.assertEqual(expected_h, distances_h)
