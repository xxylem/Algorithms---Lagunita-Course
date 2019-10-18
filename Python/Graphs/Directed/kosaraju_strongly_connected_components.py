def kosaraju(digraph):
    """ Input: directed graph G = (V, E) in adjacency-list representation, with V = {1, 2, 3, . . . , n}.
        Output: strongly_connected_components, a dictionary with
                    - keys the vertices in V
                    - values the number of the strongly connected component s.t.
                        all v's in one scc have the same number.
        Postcondition: for every v, w 2 V , scc(v) = scc(w) if and only if v, w are in the same SCC of G. """

    digraph_rev = digraph.reverse()
    # TODO new topo sort that returns list instead of dict.
    ordering = topological_sort_to_list(digraph_rev)

    explored = {}
    for v in digraph.get_vertices():
        explored[v] = False
    strongly_connected_components = {}

    num_scc = 0

    def helper(search_vertex):
        explored[search_vertex] = True
        strongly_connected_components[search_vertex] = num_scc
        for ie in search_vertex.get_incident_edges():
            new_v = ie.get_to()
            if not explored[new_v]:
                helper(new_v)

    for v in ordering:
        if not explored[v]:
            num_scc += 1
            helper(v)

    return strongly_connected_components
