def depth_first_search_recursive(graph, search_vertex):
    """ Input: graph G = (V, E) in adjacency-list representation, and a vertex s in V.
            Output: explored, a dictionary with:
                        - keys v in V
                        - values True if v is reachable from search_vertex (v and search_vertex are connected).
                                 False if v is not reachable from search_vertex (v and search_vertex are not connected).
           Postcondition: a vertex is reachable from s if and only if it is marked as “explored.”"""

    explored = {}
    for v in graph.get_vertices():
        explored[v] = False

    def helper(s):
        explored[s] = True
        for ie in s.get_incident_edges():
            v1 = ie.get_vertex1()
            v2 = ie.get_vertex2()
            if s is v1:
                new_v = v2
            else:
                new_v = v1
            if not explored[new_v]:
                helper(new_v)

    helper(search_vertex)
    return explored

#    // all vertices unexplored before outer call
# mark s as explored
# for each edge (s, v) in s’s adjacency list do
# if v is unexplored then
# DFS (G, v)


    pass


