import unittest

from Graphs.Model.graph import Graph
from Graphs.undirected_connected_components import undirected_connected_components


class UndirectedConnectedComponentsTest(unittest.TestCase):

    def test_one_node_graph(self):
        g = Graph()
        v = g.add_vertex()

        connected_components = undirected_connected_components(g)
        expected = {
            v: 1
        }

        self.assertEqual(expected, connected_components)

    def test_two_nodes_unconnected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()

        connected_components = undirected_connected_components(g)

        self.assertNotEqual(connected_components[v1], connected_components[v2])

    def test_two_nodes_connected(self):
        g = Graph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        g.add_edge(v1, v2)

        connected_components = undirected_connected_components(g)

        self.assertEqual(connected_components[v1], connected_components[v2])

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

        connected_components = undirected_connected_components(grf)

        for v in {a, b, c, d, e, g, h}:
            for v2 in {a, b, c, d, e, g, h}:
                self.assertEqual(connected_components[v], connected_components[v2])

        for v in {a, b, c, d, e, g, h}:
            self.assertNotEqual(connected_components[f], connected_components[v])


if __name__ == '__main__':
    unittest.main()
