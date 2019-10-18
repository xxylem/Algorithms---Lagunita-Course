import math
from queue import SimpleQueue


def shortest_path_breadth_first_search(graph, search_vertex):
    """ Input: graph G = (V, E) in adjacency-list representation, and a vertex s in V .
        Output: distances, a dictionary with:
                        - keys v in V
                        - values The distance dist(search_vertex, v).
        Postcondition:  for every vertex v in V , the value distances[v]
                            equals the true shortest-path distance dist(s, v)
        NOTE: dist(search_vertex, v) is NaN when v is unreachable from search_vertex. """

    # Mark all nodes as unexplored
    explored = {}
    distances = {}
    for v in graph.get_vertices():
        explored[v] = False
        distances[v] = math.inf

    # Mark search_node as explored
    explored[search_vertex] = True
    distances[search_vertex] = 0

    queue = SimpleQueue()
    queue.put(search_vertex)

    while not queue.empty():

        # Remove v from front of queue and check all its incident edges for unexplored vertices.
        v = queue.get()

        for neighbour in v.get_neighbours():
            if not explored[neighbour]:
                explored[neighbour] = True
                distances[neighbour] = distances[v] + 1
                queue.put(neighbour)

    return distances
