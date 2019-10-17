from queue import SimpleQueue


def undirected_connected_components(graph):

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

    return connected_components
