from ListTools.list_tools import choose_random_pivot, partition_list_with_pivot_at_start, swap_two_elements_by_indices


def r_select(integers, i):
    """ Input: array A of n >= 1 distinct numbers, and an
                integer i in {1, 2, . . . , n}.
        Output: the ith order statistic of A. """

    if not integers:
        raise ValueError("Input array must be nonempty.")

    if not 0 <= i < len(integers):
        raise IndexError("The ith element must be in the range (0 - length(integers) - 1)")

    def helper(left, right):
        if left >= right:
            return integers[left]

        pivot_index = choose_random_pivot(left, right)
        swap_two_elements_by_indices(integers, left, pivot_index)
        j = partition_list_with_pivot_at_start(integers, left, right)

        if j == i:
            return integers[j]
        elif j > i:
            return helper(left, j - 1)
        else:
            return helper(j + 1, right)

    return helper(0, len(integers) - 1)

