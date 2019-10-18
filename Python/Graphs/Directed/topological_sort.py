def topological_sort(dag):
    """ Input: directed acyclic graph G = (V, E) in adjacency-list representation.
        Output: ordering, a dictionary with
                    - keys all the nodes in the DAG
                    - values the assigned order for the node.
        Postcondition: the f-values of vertices constitute a topological ordering of G."""

    # Set up
    explored = {}
    for v in dag.get_vertices():
        explored[v] = False
    ordering = {}

    current_label = len(dag.get_vertices())

    # DFS recursive search helper
    def helper(search_vertex):

        explored[search_vertex] = True

        for ie in search_vertex.get_incident_edges():

            # Go deep first
            new_v = ie.get_to()
            if not explored[new_v]:
                helper(new_v)

        # Give this vertex an ordering
        nonlocal current_label
        ordering[search_vertex] = current_label
        current_label -= 1

    # Search every vertex in the DAG
    for v in dag.get_vertices():
        if not explored[v]:
            helper(v)

    return ordering


