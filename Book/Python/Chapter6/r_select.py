import random


def r_select(integers, index):
    """ Input: array A of n >= 1 distinct numbers, and an
                integer i in {1, 2, . . . , n}.
        Output: the ith order statistic of A. """

    if not integers:
        raise ValueError("Input array must be nonempty.")

    integers_length = len(integers)
    pivot = random.randint(0, integers_length)

    


    if integers_length == 1:
        return integers[0]



