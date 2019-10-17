from queue import SimpleQueue


def breadth_first_search(graph, search_vertex):
    """ Input: graph G = (V, E) in adjacency-list representation, and a vertex s in V .
        Output: explored, a dictionary with:
                    - keys v in V
                    - values True if v is reachable from search_vertex (v and search_vertex are connected).
                             False if v is not reachable from search_vertex (v and search_vertex are not connected).
        Postcondition: a vertex is reachable from s if and only if it is marked as “explored.” """

    # Mark all nodes as unexplored
    explored = {}
    for v in graph.get_vertices():
        explored[v] = False

    # Mark search_node as explored
    explored[search_vertex] = True

    queue = SimpleQueue()
    queue.put(search_vertex)

    while not queue.empty():

        # Remove v from front of queue and check all its incident edges for unexplored vertices.
        v = queue.get()
        incident_edges = v.get_incident_edges()
        for ie in incident_edges:
            # One of the vertices is the current vertex. We only want the new vertex.
            v1 = ie.get_vertex1()
            v2 = ie.get_vertex2()
            if v1 == v:
                new_v = v2
            else:
                new_v = v1

            if not explored[new_v]:
                explored[new_v] = True
                queue.put(new_v)

    return explored
