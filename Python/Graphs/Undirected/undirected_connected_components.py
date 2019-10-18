from queue import SimpleQueue


def undirected_connected_components(graph):
    """ Input: undirected graph G = (V, E) in adjacency-list representation, with V = {1, 2, 3, . . . , n}.
        Postcondition: for every u, v 2 V , cc(u) = cc(v) if and only if u, v are in the same connected component"""

    # Mark all nodes as unexplored
    explored = {}
    connected_components = {}
    for v in graph.get_vertices():
        explored[v] = False
        connected_components[v] = None

    num_connected_comp = 0

    for search_vertex in graph.get_vertices():

        if not explored[search_vertex]:

            # Mark search_node as explored
            explored[search_vertex] = True
            num_connected_comp += 1

            queue = SimpleQueue()
            queue.put(search_vertex)

            while not queue.empty():
                # Remove v from front of queue and check all its incident edges for unexplored vertices.
                v = queue.get()
                connected_components[v] = num_connected_comp

                for neighbour in v.get_neighbours():
                    if not explored[neighbour]:
                        explored[neighbour] = True
                        queue.put(neighbour)

    return connected_components
