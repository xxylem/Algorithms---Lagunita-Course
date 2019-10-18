def depth_first_search_iterative(graph, search_vertex):
    """ Input: graph G = (V, E) in adjacency-list representation, and a vertex s in V.
        Output: explored, a dictionary with:
                    - keys v in V
                    - values True if v is reachable from search_vertex (v and search_vertex are connected).
                             False if v is not reachable from search_vertex (v and search_vertex are not connected).
       Postcondition: a vertex is reachable from s if and only if it is marked as “explored.”"""

    # Setup for DFS
    explored = {}
    for v in graph.get_vertices():
        explored[v] = False

    # The stack stores the vertices that still need to be searched.
    stack = [search_vertex]

    while len(stack) > 0:
        v = stack.pop()

        if not explored[v]:
            explored[v] = True

            for neighbour in v.get_neighbours():
                stack.append(neighbour)

    return explored
