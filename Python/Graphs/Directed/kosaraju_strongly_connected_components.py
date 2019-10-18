from Graphs.Directed.topological_sort import topological_sort_to_list


def kosaraju(digraph):
    """ Input: directed graph G = (V, E) in adjacency-list representation, with V = {1, 2, 3, . . . , n}.
        Output: strongly_connected_components, a dictionary with
                    - keys the vertices in V
                    - values the number of the strongly connected component s.t.
                        all v's in one scc have the same number.
        Postcondition: for every v, w 2 V , scc(v) = scc(w) if and only if v, w are in the same SCC of G. """

    digraph_rev = digraph.reverse()
    # Ordering returns a list of vertices from digraph_rev. These are necessarily copies of the original vertices.
    # They have the same name, but they are NOT the same objects.
    ordering = topological_sort_to_list(digraph_rev)

    # Setup for DFS
    explored = {}
    for v in digraph.get_vertices():
        explored[v] = False

    # Setup to find strongly connected components
    strongly_connected_components = {}
    num_scc = 0

    # DFS from given vertex: If any vertex connected to this vertex has not yet been explored,
    # it will be found by helper and marked as belonging to this strongly connected component.
    def helper(search_vertex):
        explored[search_vertex] = True
        strongly_connected_components[search_vertex] = num_scc
        for ie in search_vertex.get_incident_edges():
            new_v = ie.get_to()
            if not explored[new_v]:
                helper(new_v)

    # Go through the vertices using the "magic" order
    for v_rev in ordering:
        # Get the vertex in the original graph that is equivalent to that in the reversed graph.
        v_forward = digraph.get_vertex(v_rev.get_name())
        if not explored[v_forward]:
            num_scc += 1
            helper(v_forward)

    return strongly_connected_components
