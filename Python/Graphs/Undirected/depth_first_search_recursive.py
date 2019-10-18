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
        for neighbour in s.get_neighbours():
            if not explored[neighbour]:
                helper(neighbour)

    helper(search_vertex)
    return explored

