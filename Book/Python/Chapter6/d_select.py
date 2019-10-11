from ListTools.list_tools import choose_random_pivot, partition_subsection_of_list_with_pivot_at_left, swap_two_elements_by_indices


def median_of_five_or_less_element_subarray(integers, left, right):
    integers_copy = integers.copy()
    integers_copy.sort()
    return integers_copy[(left + right) // 2] ## TODO move back into main function


def d_select(integers, i):
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

        size_sublist = 1 + right - left
        #
        # indices = [x for x in range(0, size_sublist) if x % 5 == 2]
        # if not indices:
        #     indices = [size_sublist // 2]
        #
        # medians = []
        #
        # for h in range(len(indices)):
        #     medians.append(integers[indices[h]])

        medians = []
        start = left
        end = min(right, start + 5)
        while start != end:
            medians.append(median_of_five_or_less_element_subarray(integers, start, end))
            temp = start
            start = end
            end = min(right, temp + 5) ## TODO or move this up to the medians

        pivot = d_select(medians, min(size_sublist // 10, len(medians) - 1))
        pivot_index = integers.index(pivot) ## TODO this is bad, can we keep track of the pivot index?

        # Move the pivot element to the left of the sublist.
        swap_two_elements_by_indices(integers, left, pivot_index)

        # Update the pivot index after partitioning the sublist around the pivot element.
        pivot_index = partition_subsection_of_list_with_pivot_at_left(integers, left, right)

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
