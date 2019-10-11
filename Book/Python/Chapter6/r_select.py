from ListTools.list_tools import choose_random_pivot, partition_subsection_of_list_using_pivot_index


def r_select(integers, i):
    """ Input: array A of n >= 1 distinct numbers, and an
                integer i in {1, 2, . . . , n}.
        Output: the ith order statistic of A. """

    if not integers:
        # RSelect is undefined on empty lists.
        raise ValueError("Input array must be nonempty.")

    if not 0 <= i < len(integers):
        # Handles attempts to use negative indexing
        # or requests for the ith order element where i is not in the appropriate range.
        raise IndexError("The ith element must be in the range [0, length(integers) - 1]")

    # helper operates on a subsection of the integers list, between left and right, inclusive.
    def helper(left, right):

        # 1 element: return it.
        if left >= right:
            return integers[left]

        pivot_index = choose_random_pivot(left, right)

        # Update the pivot index after partitioning the sublist around the pivot element.
        pivot_index = partition_subsection_of_list_using_pivot_index(integers, left, right, pivot_index)

        # Found the ith order element.
        if pivot_index == i:
            return integers[pivot_index]

        # The ith order element is smaller than the pivot element.
        elif pivot_index > i:
            return helper(left, pivot_index - 1)

        # The ith order element is larger than the pivot element.
        else:
            return helper(pivot_index + 1, right)

    return helper(0, len(integers) - 1)

