import unittest

from Graphs.Directed.Model.digraph import DirectedGraph
from Graphs.Directed.topological_sort import topological_sort


class TopologicalSortTest(unittest.TestCase):

    def test_one_node_graph(self):
        dag = DirectedGraph()
        v = dag.add_vertex()

        ordering = topological_sort(dag)

        self.assertEqual(ordering[v], 1)
        self.assertEqual(len(ordering), 1)

    def test_two_nodes_unconnected(self):
        dag = DirectedGraph()
        dag.add_vertex()
        dag.add_vertex()
        ordering = topological_sort(dag)

        # We cannot check the specific order since either vertex can be first.
        self.assertEqual(len(ordering), 2)

    def test_two_nodes_connected(self):
        dag = DirectedGraph()
        v1 = dag.add_vertex()
        v2 = dag.add_vertex()
        dag.add_edge(v1, v2)

        ordering = topological_sort(dag)

        self.assertLess(ordering[v1], ordering[v2])

    def test_on_larger_graph(self):
        dag = DirectedGraph()

        a = dag.add_vertex()
        b = dag.add_vertex()
        c = dag.add_vertex()
        d = dag.add_vertex()
        e = dag.add_vertex()
        f = dag.add_vertex()
        g = dag.add_vertex()
        h = dag.add_vertex()
        i = dag.add_vertex()
        j = dag.add_vertex()

        dag.add_edge(a, g)
        dag.add_edge(a, d)
        dag.add_edge(d, h)
        dag.add_edge(d, e)
        dag.add_edge(h, b)
        dag.add_edge(c, h)
        dag.add_edge(i, j)

        ordering = topological_sort(dag)

        self.assertEqual(len(ordering), 10)

        self.assertLess(ordering[a], ordering[g])
        self.assertLess(ordering[a], ordering[d])
        self.assertLess(ordering[a], ordering[e])
        self.assertLess(ordering[a], ordering[h])
        self.assertLess(ordering[a], ordering[b])

        self.assertLess(ordering[d], ordering[e])
        self.assertLess(ordering[d], ordering[h])
        self.assertLess(ordering[d], ordering[b])

        self.assertLess(ordering[c], ordering[h])
        self.assertLess(ordering[c], ordering[b])

        self.assertLess(ordering[h], ordering[b])

        self.assertLess(ordering[i], ordering[j])


if __name__ == '__main__':
    unittest.main()
