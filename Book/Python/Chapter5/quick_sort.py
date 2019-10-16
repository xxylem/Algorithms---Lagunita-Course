from ListTools.partition import swap_two_elements_by_indices, partition_subsection_of_list_with_pivot_at_left
from ListTools.choose_pivot import choose_random_pivot


def quick_sort(integers):
    """ Input: list of n distinct integers
        Side-effect: elements of the list are sorted from smallest to largest."""
    quick_sort_with_pivot_function(integers, choose_random_pivot)


def quick_sort_with_pivot_function(integers, pivot_function):

    def helper(left, right):

        # Used to avoid needing left and right params in the main function.
        # The helper function works on the subarray that is integers between indices left and right, inclusive.

        if left >= right:
            # 0 or 1 element list, no sorting to do.
            return 0

        # Select a pivot and move it to the front of the subarray.
        i = pivot_function(left, right, integers)
        swap_two_elements_by_indices(integers, left, i)

        # Partition the subarray based on the pivot
        j = partition_subsection_of_list_with_pivot_at_left(integers, left, right)

        # Recursively sort the subarrays either side of the pivot.
        comparisons_left = helper(left, j - 1)
        comparisons_right = helper(j + 1, right)

        return comparisons_left + comparisons_right + right - left

    # Initially call the helper function with the full range of the array, using inclusive indices with 0-indexing.
    return helper(0, len(integers) - 1)
