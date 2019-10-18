import unittest

from Graphs.Directed.Model.digraph import DirectedGraph
from Graphs.Directed.kosaraju_strongly_connected_components import kosaraju


class KosarajuTest(unittest.TestCase):

    def test_one_node_graph(self):
        g = DirectedGraph()
        v = g.add_vertex()

        connected_components = kosaraju(g)
        expected = {
            v: 1
        }

        self.assertEqual(expected, connected_components)

    def test_two_nodes_unconnected(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()

        connected_components = kosaraju(g)

        self.assertNotEqual(connected_components[v1], connected_components[v2])

    def test_two_nodes_connected_one_way(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        g.add_edge(v1, v2)

        connected_components = kosaraju(g)

        self.assertNotEqual(connected_components[v1], connected_components[v2])

    def test_two_nodes_strongly_connected(self):
        g = DirectedGraph()
        v1 = g.add_vertex()
        v2 = g.add_vertex()
        g.add_edge(v1, v2)
        g.add_edge(v2, v1)

        connected_components = kosaraju(g)

        self.assertEqual(connected_components[v1], connected_components[v2])

    def test_on_larger_graph(self):
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

        grf.add_edge(a, g)
        grf.add_edge(a, d)
        grf.add_edge(d, a)
        grf.add_edge(d, h)
        grf.add_edge(d, e)
        grf.add_edge(e, d)
        grf.add_edge(h, b)
        grf.add_edge(c, h)
        grf.add_edge(i, j)

        connected_components = kosaraju(grf)

        all_vs = {a, b, c, d, e, f, g, h, i, j}
        comp1 = {a, d, e}
        comp2 = {g}
        comp3 = {f}
        comp4 = {i}
        comp5 = {j}
        comp6 = {c}
        comp7 = {h}
        comp8 = {b}
        all_comps = [comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8]

        for comp in all_comps:
            for c in comp:
                for other in (comp - c):
                    self.assertEqual(connected_components[c], connected_components[other])
                for other in (all_vs - comp):
                    self.assertNotEqual(connected_components[c], connected_components[other])


if __name__ == '__main__':
    unittest.main()
