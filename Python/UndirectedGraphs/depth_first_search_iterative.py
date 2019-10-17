def depth_first_search_iterative(graph, search_vertex):
    """ Input: graph G = (V, E) in adjacency-list representation, and a vertex s in V.
        Output: explored, a dictionary with:
                    - keys v in V
                    - values True if v is reachable from search_vertex (v and search_vertex are connected).
                             False if v is not reachable from search_vertex (v and search_vertex are not connected).
       Postcondition: a vertex is reachable from s if and only if it is marked as “explored.”"""

    explored = {}
    for v in graph.get_vertices():
        explored[v] = False

    stack = [search_vertex]

    while len(stack) > 0:
        v = stack.pop()

        if not explored[v]:
            explored[v] = True

            for ie in v.get_incident_edges():
                v1 = ie.get_vertex1()
                v2 = ie.get_vertex2()
                if v is v1:
                    new_v = v2
                else:
                    new_v = v1
                stack.append(new_v)

    return explored