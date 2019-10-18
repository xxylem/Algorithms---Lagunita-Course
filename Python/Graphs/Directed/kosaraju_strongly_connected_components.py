def kosaraju(digraph):
    """ Input: directed graph G = (V, E) in adjacency-list representation, with V = {1, 2, 3, . . . , n}.
        Output: strongly_connected_components, a dictionary with
                    - keys the vertices in V
                    - values the number of the strongly connected component s.t.
                        all v's in one scc have the same number.
        Postcondition: for every v, w 2 V , scc(v) = scc(w) if and only if v, w are in the same SCC of G. """

    digraph_rev = digraph.reverse()

    explored = {}
    for v in digraph_rev.get_vertices():
        


# mark all vertices of Grev as unexplored
# // first pass of depth-first search
# // (computes f(v)’s, the magical ordering)
# TopoSort (Grev)
# // second pass of depth-first search
# // (finds SCCs in reverse topological order)
# mark all vertices of G as unexplored
# numSCC := 0 // global variable
# for each v 2 V , in increasing order of f(v) do
# if v is unexplored then
# numSCC := numSCC + 1
# // assign scc-values (details below)
# DFS-SCC (G, v)"""
