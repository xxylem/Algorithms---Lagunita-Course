from queue import SimpleQueue


def breadth_first_search(graph, s):

    explored = {}

    for v in graph.get_vertices():
        explored[v] = False

    explored[s] = True

    queue = SimpleQueue()
    queue.put(s)

